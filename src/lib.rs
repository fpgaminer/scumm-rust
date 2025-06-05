mod ast;
mod interpreter;
mod preprocessor;

use anyhow::Result;
use ast::{
	Block, Class, Expression, FunctionCall, IfStatement, Object, Primary, PropertyAssignment, Room, Script, Statement, TopLevel, VariableDeclaration,
	VerbStatement, WhileStatement,
};
use log::{debug, error, info};
use pest::Parser;
use preprocessor::preprocess;
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::{JsFuture, spawn_local};
use web_sys::Response;


#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
struct ScummParser;

use pest::iterators::Pair;

fn unquote(raw: &str) -> &str {
	&raw[1..raw.len() - 1]
}

fn parse_block(pair: Pair<Rule>) -> Block {
	let mut statements = Vec::new();
	for stmt_pair in pair.into_inner() {
		if let Some(statement) = parse_statement(stmt_pair) {
			statements.push(statement);
		}
	}
	Block { statements }
}

fn parse_block_filtered(pair: Pair<Rule>, allowed: &[Rule], context: &str) -> Result<Block, String> {
	let mut statements = Vec::new();
	for stmt_pair in pair.into_inner() {
		if allowed.contains(&stmt_pair.as_rule()) {
			if let Some(statement) = parse_statement(stmt_pair) {
				statements.push(statement);
			}
		} else {
			return Err(format!("Unexpected statement '{:?}' in {}", stmt_pair.as_rule(), context));
		}
	}
	Ok(Block { statements })
}

fn parse_def_block(pair: Pair<Rule>) -> Result<Block, String> {
	parse_block_filtered(pair, &[Rule::verb_stmt, Rule::prop_assign], "def_block, expected: verb_stmt, or prop_assign")
}

fn parse_binary_expr<O, F>(pair: Pair<Rule>, map_op: fn(&str) -> O, make_expr: F) -> Expression
where
	F: Fn(Box<Expression>, O, Box<Expression>) -> Expression,
{
	let mut inner = pair.into_inner();
	let left = parse_expression(inner.next().unwrap());
	if let Some(op_pair) = inner.next() {
		let right_pair = inner.next().unwrap();
		let op = map_op(op_pair.as_str());
		let right_expr = parse_expression(right_pair);
		make_expr(Box::new(left), op, Box::new(right_expr))
	} else {
		left
	}
}

fn parse_room_block(pair: Pair<Rule>) -> Result<Block, String> {
	parse_block_filtered(
		pair,
		&[Rule::prop_assign, Rule::verb_stmt, Rule::object_def, Rule::script_def],
		"room_block, expected: prop_assign, verb_stmt, object_def, or script_def",
	)
}

fn parse_statement(pair: Pair<Rule>) -> Option<Statement> {
	match pair.as_rule() {
		Rule::block => Some(Statement::Block(parse_block(pair))),
		Rule::expr_stmt => {
			let mut inner = pair.into_inner();
			inner.next().map(|expr_pair| Statement::Expression(parse_expression(expr_pair)))
		},
		Rule::if_stmt => {
			let mut inner = pair.into_inner();
			let condition = parse_expression(inner.next()?);
			let then_block = parse_block(inner.next()?);
			let else_block = inner.next().map(|p| {
				if p.as_rule() == Rule::if_stmt {
					// else if: parse as a nested if statement wrapped in a block
					let stmt = parse_statement(p).expect("nested if");
					Block { statements: vec![stmt] }
				} else {
					parse_block(p)
				}
			});
			Some(Statement::If(IfStatement {
				condition,
				then_block,
				else_block,
			}))
		},
		Rule::while_stmt => {
			let mut inner = pair.into_inner();
			let condition = parse_expression(inner.next()?);
			let body = parse_block(inner.next()?);
			Some(Statement::While(WhileStatement { condition, body }))
		},
		Rule::control_stmt => {
			// Control statements contain if_stmt or while_stmt as children
			let mut inner = pair.into_inner();
			if let Some(inner_pair) = inner.next() {
				parse_statement(inner_pair)
			} else {
				None
			}
		},
		Rule::verb_stmt => {
			let mut inner = pair.into_inner();
			let name = inner.next()?.as_str().to_string();
			// Skip the param_list for now (we don't store parameters in the AST yet)
			let _param_list = inner.next()?;
			let body = inner.next().map(parse_block);
			Some(Statement::Verb(VerbStatement { name, body }))
		},
		Rule::var_decl => {
			let mut inner = pair.into_inner();
			let var_type = inner.next()?.as_str().to_string();
			let name = inner.next()?.as_str().to_string();
			let value = parse_expression(inner.next()?);
			Some(Statement::VariableDeclaration(VariableDeclaration {
				var_type: var_type.parse().unwrap(),
				name,
				value,
			}))
		},
		Rule::prop_assign => {
			let mut inner = pair.into_inner();
			let name = inner.next()?.as_str().to_string();
			let value_pair = inner.next()?;
			let value = parse_expression(value_pair.clone());
			Some(Statement::PropertyAssignment(PropertyAssignment { name, value }))
		},
		Rule::object_def => {
			let mut inner = pair.into_inner();
			let id = inner.next()?.as_str().to_string();
			let body = match inner.next().map(parse_def_block) {
				Some(Ok(block)) => block,
				Some(Err(_)) => return None, // Could log error here
				None => Block { statements: vec![] },
			};
			Some(Statement::ObjectDeclaration(Object { id, body }))
		},
		Rule::script_def => {
			let mut inner = pair.into_inner();
			let name = inner.next()?.as_str().to_string();
			let _params = inner.next()?; // param_list
			let body = inner.next().map(parse_block).unwrap_or(Block { statements: vec![] });
			Some(Statement::ScriptDeclaration(Script { name, body }))
		},
		_ => None,
	}
}

fn parse_expression(pair: Pair<Rule>) -> Expression {
	match pair.as_rule() {
		Rule::assignment => {
			let mut inner = pair.into_inner();
			let left = parse_expression(inner.next().unwrap());
			if let Some(right) = inner.next() {
				Expression::Assignment(Box::new(left), Box::new(parse_expression(right)))
			} else {
				left
			}
		},
		Rule::logical_or => parse_binary_expr(
			pair,
			|_| (), // operator has no enum, handled directly
			|l, _, r| Expression::LogicalOr(l, r),
		),
		Rule::logical_and => parse_binary_expr(pair, |_| (), |l, _, r| Expression::LogicalAnd(l, r)),
		Rule::equality => parse_binary_expr(
			pair,
			|op| match op {
				"==" => ast::EqualityOp::Equal,
				"!=" => ast::EqualityOp::NotEqual,
				_ => ast::EqualityOp::Equal,
			},
			Expression::Equality,
		),
		Rule::comparison => parse_binary_expr(
			pair,
			|op| match op {
				"<" => ast::ComparisonOp::Less,
				">" => ast::ComparisonOp::Greater,
				"<=" => ast::ComparisonOp::LessEqual,
				">=" => ast::ComparisonOp::GreaterEqual,
				_ => ast::ComparisonOp::Less,
			},
			Expression::Comparison,
		),
		Rule::term => parse_binary_expr(
			pair,
			|op| match op {
				"+" => ast::TermOp::Add,
				"-" => ast::TermOp::Subtract,
				_ => ast::TermOp::Add,
			},
			Expression::Term,
		),
		Rule::factor => parse_binary_expr(
			pair,
			|op| match op {
				"*" => ast::FactorOp::Multiply,
				"/" => ast::FactorOp::Divide,
				_ => ast::FactorOp::Multiply,
			},
			Expression::Factor,
		),
		Rule::unary => {
			let pair_clone = pair.clone();
			let mut inner = pair.into_inner();
			let expr_pair = inner.next().expect("unary expression missing primary");

			// Determine unary operators from the prefix before the
			// primary expression.
			let span = pair_clone.as_span();
			let expr_span = expr_pair.as_span();
			let prefix_len = expr_span.start() - span.start();
			let prefix = &pair_clone.as_str()[..prefix_len];

			let mut ops = Vec::new();
			for ch in prefix.chars() {
				match ch {
					'!' => ops.push(ast::UnaryOp::Not),
					'-' => ops.push(ast::UnaryOp::Negate),
					_ => {},
				}
			}

			let mut expr = Expression::Primary(parse_primary(expr_pair));

			// Apply unary operators from right to left
			for op in ops.into_iter().rev() {
				expr = Expression::Unary(op, Box::new(expr));
			}

			expr
		},
		Rule::primary => Expression::Primary(parse_primary(pair)),
		_ => {
			// For any other rule, try to find a primary within it
			let mut inner = pair.into_inner();
			if let Some(first) = inner.next() {
				parse_expression(first)
			} else {
				Expression::Primary(Primary::Number(0))
			}
		},
	}
}

fn parse_primary(pair: Pair<Rule>) -> Primary {
	match pair.as_rule() {
		Rule::array_literal => {
			let elements = pair.into_inner().map(parse_expression).collect();
			Primary::Array(elements)
		},
		Rule::number => Primary::Number(pair.as_str().parse().unwrap_or(0)),
		Rule::string => Primary::String(unquote(pair.as_str()).to_string()),
		Rule::identifier => Primary::Identifier(pair.as_str().to_string()),
		Rule::func_call => {
			let mut inner = pair.into_inner();
			let name = inner.next().unwrap().as_str().to_string();
			let mut arguments = Vec::new();
			if let Some(arg_list) = inner.next() {
				for arg_pair in arg_list.into_inner() {
					arguments.push(parse_expression(arg_pair));
				}
			}
			Primary::FunctionCall(FunctionCall { name, arguments })
		},
		Rule::primary => {
			// Handle the case where primary contains other rules
			let mut inner = pair.into_inner();
			if let Some(inner_pair) = inner.next() {
				parse_primary(inner_pair)
			} else {
				Primary::Number(0) // Fallback
			}
		},
		Rule::expr => {
			// Parenthesized expression
			let inner_expr = pair.into_inner().next().unwrap();
			Primary::Parenthesized(Box::new(parse_expression(inner_expr)))
		},
		_ => Primary::Parenthesized(Box::new(parse_expression(pair))),
	}
}


pub fn parse_str(input: &str) -> Result<Vec<TopLevel>> {
	let pairs = ScummParser::parse(Rule::file, input)?;
	let mut ast_nodes = Vec::new();

	for pair in pairs {
		if pair.as_rule() == Rule::file {
			for inner_pair in pair.into_inner() {
				match inner_pair.as_rule() {
					Rule::item => {
						for item_pair in inner_pair.into_inner() {
							match item_pair.as_rule() {
								Rule::script_def => {
									let mut script_inner = item_pair.into_inner();
									let name_pair = script_inner.next().unwrap();
									let name = name_pair.as_str();

									// Skip the param_list for now (we don't store parameters in the AST yet)
									let _param_list = script_inner.next().unwrap();

									let body = if let Some(block_pair) = script_inner.next() {
										parse_block(block_pair)
									} else {
										Block { statements: Vec::new() }
									};

									ast_nodes.push(TopLevel::Script(Script { name: name.to_string(), body }));
								},
								Rule::object_def => {
									let mut object_inner = item_pair.into_inner();
									let id = object_inner.next().unwrap().as_str().to_string();
									let body = if let Some(block_pair) = object_inner.next() {
										match parse_def_block(block_pair) {
											Ok(block) => block,
											Err(err) => return Err(anyhow::anyhow!("Error parsing object def_block: {}", err)),
										}
									} else {
										Block { statements: Vec::new() }
									};

									ast_nodes.push(TopLevel::Object(Object { id, body }));
								},
								Rule::class_def => {
									let mut class_inner = item_pair.into_inner();
									let name = class_inner.next().unwrap().as_str().to_string();
									let body = if let Some(block_pair) = class_inner.next() {
										match parse_def_block(block_pair) {
											Ok(block) => block,
											Err(err) => return Err(anyhow::anyhow!("Error parsing class def_block: {}", err)),
										}
									} else {
										Block { statements: Vec::new() }
									};

									ast_nodes.push(TopLevel::Class(Class { name, body }));
								},
								Rule::room_def => {
									let mut room_inner = item_pair.into_inner();
									let id = room_inner.next().unwrap().as_str().to_string();
									let body = if let Some(block_pair) = room_inner.next() {
										match parse_room_block(block_pair) {
											Ok(block) => block,
											Err(err) => return Err(anyhow::anyhow!("Error parsing room room_block: {}", err)),
										}
									} else {
										Block { statements: Vec::new() }
									};

									ast_nodes.push(TopLevel::Room(Room { id, body }));
								},
								_ => {},
							}
						}
					},
					Rule::EOI => {},
					_ => {},
				}
			}
		}
	}

	Ok(ast_nodes)
}


pub async fn load_script_raw() -> Result<String, JsValue> {
	let resp_value = JsFuture::from(window().fetch_with_str("/example.sc")).await?;

	let resp: Response = resp_value.dyn_into()?;
	let text = JsFuture::from(resp.text()?).await?;
	Ok(text.as_string().unwrap())
}


#[wasm_bindgen]
pub fn start() {
	set_panic_hook();
	init_logging();
	info!("Starting Scumm Script Parser...");
	spawn_local(async {
		match load_script_raw().await {
			Ok(source) => {
				handle_script(source.as_bytes())
					.map_err(|e| error!("Script handling error: {}", e))
					.unwrap_or(());
			},
			Err(err) => error!("Failed to load script: {:?}", err),
		}
	});
}


fn handle_script(data: &[u8]) -> Result<(), anyhow::Error> {
	// Phase 1: preprocess includes.
	let flattened = preprocess("main.sc", &String::from_utf8_lossy(data))?;
	debug!("Preprocessed content length: {} chars", flattened.len());

	// Phase 2: parse and build AST.
	let ast_nodes = match parse_str(&flattened) {
		Ok(ast) => ast,
		Err(e) => {
			error!("Parse error: {}", e);
			error!("Preprocessed content preview:");
			let lines: Vec<&str> = flattened.lines().collect();
			for (i, line) in lines.iter().enumerate().take(50) {
				error!("{:3}: {}", i + 1, line);
			}
			return Err(anyhow::anyhow!(e));
		},
	};

	debug!("Parsed successfully! AST nodes: {:?}", ast_nodes);

	// Run the interpreter with web interface
	let interpreter = interpreter::Interpreter::new(&ast_nodes);

	let tick_closure = {
		Closure::<dyn FnMut()>::new(move || {
			interpreter.tick_web();
		})
	};

	// one tick per second (for debugging purposes)
	window()
		.set_interval_with_callback_and_timeout_and_arguments_0(tick_closure.as_ref().unchecked_ref(), 1000)
		.expect("should register `setInterval` OK");

	tick_closure.forget(); // Prevent closure from being dropped

	//let f = Rc::new(RefCell::new(None));
	//let g = f.clone();

	//*g.borrow_mut() = Some(Closure::new(move || {
	//	interpreter.tick_web();

	//	request_animation_frame(f.borrow().as_ref().unwrap());
	//}));

	//request_animation_frame(g.borrow().as_ref().unwrap());

	Ok(())
}


fn window() -> web_sys::Window {
	web_sys::window().expect("no global `window` exists")
}

#[allow(dead_code)]
fn request_animation_frame(f: &Closure<dyn FnMut()>) {
	window()
		.request_animation_frame(f.as_ref().unchecked_ref())
		.expect("should register `requestAnimationFrame` OK");
}


pub fn set_panic_hook() {
	// When the `console_error_panic_hook` feature is enabled, we can call the
	// `set_panic_hook` function at least once during initialization, and then
	// we will get better error messages if our code ever panics.
	//
	// For more details see
	// https://github.com/rustwasm/console_error_panic_hook#readme
	#[cfg(feature = "console_error_panic_hook")]
	console_error_panic_hook::set_once();
}

fn init_logging() {
	#[cfg(target_arch = "wasm32")]
	{
		console_log::init_with_level(log::Level::Debug).expect("Failed to initialize console_log");
	}

	#[cfg(not(target_arch = "wasm32"))]
	{
		env_logger::Builder::from_default_env().filter_level(log::LevelFilter::Debug).init();
	}
}


#[cfg(test)]
mod tests {
	use super::*;
	use crate::ast::{EqualityOp, FactorOp, TermOp, VarType};
	use std::fs;

	fn parse_ok(src: &str) -> Vec<TopLevel> {
		parse_str(src).expect("parse failed")
	}

	fn parse_err(src: &str) {
		assert!(parse_str(src).is_err());
	}

	#[test]
	fn script_name_identifier_no_args() {
		parse_err("script ROOM { }\n");
	}

	#[test]
	fn script_name_identifier() {
		let ast = parse_ok("script ROOM() { }\n");
		assert_eq!(
			ast,
			vec![TopLevel::Script(Script {
				name: "ROOM".to_string(),
				body: Block { statements: vec![] },
			})]
		);
	}

	#[test]
	fn script_name_number() {
		parse_err("script 5 { }\n");
		parse_err("script 5() { }\n");
	}

	#[test]
	fn object_with_verbs() {
		let src = r#"object OBJ {
    class = {Cls};
    verb vOne();
    verb vTwo() {
        call();
    }
}"#;
		let ast = parse_ok(src);

		assert_eq!(ast.len(), 1);
		match &ast[0] {
			TopLevel::Object(obj) => {
				assert_eq!(obj.id, "OBJ");
				assert_eq!(obj.body.statements.len(), 3);

				matches!(obj.body.statements[0], Statement::PropertyAssignment(_));

				if let Statement::Verb(ref verb) = obj.body.statements[1] {
					assert_eq!(verb.name, "vOne");
					assert!(verb.body.is_none());
				} else {
					panic!("expected verb statement");
				}

				if let Statement::Verb(ref verb) = obj.body.statements[2] {
					assert_eq!(verb.name, "vTwo");
					let body = verb.body.as_ref().unwrap();
					assert_eq!(body.statements.len(), 1);
					if let Statement::Expression(Expression::Primary(Primary::FunctionCall(fc))) = &body.statements[0] {
						assert_eq!(fc.name, "call");
						assert!(fc.arguments.is_empty());
					} else {
						panic!("expected call() inside verb body");
					}
				} else {
					panic!("expected verb statement with body");
				}
			},
			_ => panic!("expected object"),
		}
	}

	#[test]
	fn while_loop() {
		let src = r#"script S() {
    while (1) {
        call();
    }
}"#;
		let ast = parse_ok(src);

		if let TopLevel::Script(script) = &ast[0] {
			assert_eq!(script.body.statements.len(), 1);
			if let Statement::While(w) = &script.body.statements[0] {
				assert_eq!(w.body.statements.len(), 1);
				if let Statement::Expression(Expression::Primary(Primary::FunctionCall(fc))) = &w.body.statements[0] {
					assert_eq!(fc.name, "call");
				} else {
					panic!("expected function call inside while");
				}
				if let Expression::Primary(Primary::Number(1)) = w.condition {
				} else {
					panic!("expected numeric condition");
				}
			} else {
				panic!("expected while statement");
			}
		} else {
			panic!("expected script");
		}
	}

	#[test]
	fn if_else_condition() {
		let src = r#"script S() {
    if (cond == 1) {
        a();
    } else {
        b();
    }
}"#;
		let ast = parse_ok(src);
		if let TopLevel::Script(script) = &ast[0] {
			if let Statement::If(stmt) = &script.body.statements[0] {
				assert!(stmt.else_block.is_some());
				if let Expression::Equality(left, op, right) = &stmt.condition {
					assert_eq!(*op, EqualityOp::Equal);
					if let Expression::Primary(Primary::Identifier(id)) = &**left {
						assert_eq!(id, "cond");
					} else {
						panic!("expected identifier left operand");
					}
					if let Expression::Primary(Primary::Number(1)) = &**right {
					} else {
						panic!("expected numeric right operand");
					}
				} else {
					panic!("expected equality condition");
				}
				let then_block = &stmt.then_block;
				assert_eq!(then_block.statements.len(), 1);
				if let Statement::Expression(Expression::Primary(Primary::FunctionCall(fc))) = &then_block.statements[0] {
					assert_eq!(fc.name, "a");
				} else {
					panic!("expected call to a() in then block");
				}
				let else_block = stmt.else_block.as_ref().unwrap();
				assert_eq!(else_block.statements.len(), 1);
				if let Statement::Expression(Expression::Primary(Primary::FunctionCall(fc))) = &else_block.statements[0] {
					assert_eq!(fc.name, "b");
				} else {
					panic!("expected call to b() in else block");
				}
			} else {
				panic!("expected if statement");
			}
		}
	}

	#[test]
	fn variable_declaration() {
		let src = r#"script S() {
    string code = prompt("hi");
}"#;
		let ast = parse_ok(src);
		if let TopLevel::Script(script) = &ast[0] {
			if let Statement::VariableDeclaration(var) = &script.body.statements[0] {
				assert_eq!(var.var_type, VarType::String);
				assert_eq!(var.name, "code");
				if let Expression::Primary(Primary::FunctionCall(fc)) = &var.value {
					assert_eq!(fc.name, "prompt");
					assert_eq!(fc.arguments.len(), 1);
					if let Expression::Primary(Primary::String(s)) = &fc.arguments[0] {
						assert_eq!(s, "hi");
					} else {
						panic!("expected string argument");
					}
				} else {
					panic!("expected function call");
				}
			} else {
				panic!("expected variable declaration");
			}
		}
	}

	#[test]
	fn class_definition() {
		let src = r#"class C {
    verb vRun() { run(); }
}"#;
		let ast = parse_ok(src);
		if let TopLevel::Class(class) = &ast[0] {
			assert_eq!(class.name, "C");
			assert_eq!(class.body.statements.len(), 1);
			if let Statement::Verb(verb) = &class.body.statements[0] {
				assert_eq!(verb.name, "vRun");
				let body = verb.body.as_ref().expect("verb body");
				assert_eq!(body.statements.len(), 1);
				if let Statement::Expression(Expression::Primary(Primary::FunctionCall(fc))) = &body.statements[0] {
					assert_eq!(fc.name, "run");
				} else {
					panic!("expected run() call in verb");
				}
			} else {
				panic!("expected verb statement");
			}
		} else {
			panic!("expected class");
		}
	}

	#[test]
	fn logical_and_equality_expression() {
		let src = r#"script S() {
    if (a() && b == c) { }
}"#;
		let ast = parse_ok(src);
		if let TopLevel::Script(script) = &ast[0] {
			if let Statement::If(stmt) = &script.body.statements[0] {
				if let Expression::LogicalAnd(left, right) = &stmt.condition {
					if let Expression::Primary(Primary::FunctionCall(fc)) = &**left {
						assert_eq!(fc.name, "a");
					} else {
						panic!("expected function call");
					}
					if let Expression::Equality(l, EqualityOp::Equal, r) = &**right {
						if let Expression::Primary(Primary::Identifier(id)) = &**l {
							assert_eq!(id, "b");
						} else {
							panic!("expected identifier left of equality");
						}
						if let Expression::Primary(Primary::Identifier(id)) = &**r {
							assert_eq!(id, "c");
						} else {
							panic!("expected identifier right of equality");
						}
					} else {
						panic!("expected equality");
					}
				} else {
					panic!("expected logical and");
				}
			}
		}
	}

	#[test]
	fn property_assignment() {
		let src = r#"object O {
    state = 0;
}"#;
		let ast = parse_ok(src);
		if let TopLevel::Object(obj) = &ast[0] {
			if let Statement::PropertyAssignment(prop) = &obj.body.statements[0] {
				assert_eq!(prop.name, "state");
				assert_eq!(prop.value, Expression::Primary(Primary::Number(0)));
			} else {
				panic!("expected property assignment");
			}
		} else {
			panic!("expected object");
		}
	}

	#[test]
	fn arithmetic_precedence() {
		let src = r#"script S() {
    a = 3 + 4 * 5;
}"#;
		let ast = parse_ok(src);
		debug!("AST: {:?}", ast);
		if let TopLevel::Script(script) = &ast[0] {
			if let Statement::Expression(Expression::Assignment(_, expr)) = &script.body.statements[0] {
				if let Expression::Term(left, op, right) = &**expr {
					assert_eq!(*op, TermOp::Add);
					if let Expression::Primary(Primary::Number(3)) = **left {
					} else {
						panic!("expected number")
					}
					if let Expression::Factor(f_left, FactorOp::Multiply, f_right) = &**right {
						if matches!(&**f_left, Expression::Primary(Primary::Number(4))) {
						} else {
							panic!("expected number 4");
						}
						if matches!(&**f_right, Expression::Primary(Primary::Number(5))) {
						} else {
							panic!("expected number 5");
						}
					} else {
						panic!("expected multiply")
					}
				} else {
					panic!("expected term expression");
				}
			} else {
				panic!("expected assignment expression");
			}
		} else {
			panic!("expected script");
		}
	}


	#[test]
	fn function_call_arguments() {
		let src = r#"script S() {
    call(1, 2, 3);
}"#;
		let ast = parse_ok(src);
		if let TopLevel::Script(script) = &ast[0] {
			if let Statement::Expression(Expression::Primary(Primary::FunctionCall(fc))) = &script.body.statements[0] {
				assert_eq!(fc.name, "call");
				assert_eq!(fc.arguments.len(), 3);
				if let Expression::Primary(Primary::Number(1)) = fc.arguments[0] {
				} else {
					panic!("expected first argument 1");
				}
				if let Expression::Primary(Primary::Number(2)) = fc.arguments[1] {
				} else {
					panic!("expected second argument 2");
				}
				if let Expression::Primary(Primary::Number(3)) = fc.arguments[2] {
				} else {
					panic!("expected third argument 3");
				}
			} else {
				panic!("expected function call");
			}
		}
	}

	#[test]
	fn parse_example_script() {
		let src = include_str!("../example.sc");
		let ast = parse_ok(src);
		assert!(!ast.is_empty());
	}

	#[test]
	fn invalid_missing_semicolon() {
		let src = "script S() { string a = 1 }";
		parse_err(src);
	}

	#[test]
	fn invalid_unclosed_block() {
		let src = "script S() {";
		parse_err(src);
	}

	#[test]
	fn room_with_entry_and_object() {
		let src = r#"room Kitchen {
    image = "kitchen.png";
    script entry() { startScript(GameLoop); }
    object Fridge {}
}"#;
		let ast = parse_ok(src);
		assert_eq!(ast.len(), 1);
		if let TopLevel::Room(room) = &ast[0] {
			assert_eq!(room.id, "Kitchen");
			assert_eq!(room.body.statements.len(), 3);
			matches!(room.body.statements[0], Statement::PropertyAssignment(_));
			matches!(room.body.statements[1], Statement::ScriptDeclaration(_));
			matches!(room.body.statements[2], Statement::ObjectDeclaration(_));
		} else {
			panic!("expected room definition");
		}
	}

	#[test]
	fn comments_are_ignored() {
		let src = r#"// single line comment
script S() {
    /* multi-line
       comment */
    call();
}
"#;
		let ast = parse_ok(src);
		if let TopLevel::Script(script) = &ast[0] {
			assert_eq!(script.name, "S");
			assert_eq!(script.body.statements.len(), 1);
			matches!(script.body.statements[0], Statement::Expression(_));
		} else {
			panic!("expected script");
		}
	}

	#[test]
	fn variable_declaration_bool_and_int() {
		let src = r#"script S() {
    int counter = 1;
    bool found = false;
}"#;
		let ast = parse_ok(src);
		if let TopLevel::Script(script) = &ast[0] {
			assert_eq!(script.body.statements.len(), 2);

			if let Statement::VariableDeclaration(var) = &script.body.statements[0] {
				assert_eq!(var.var_type, VarType::Int);
				assert!(matches!(var.value, Expression::Primary(Primary::Number(1))));
			} else {
				panic!("expected first variable declaration");
			}

			if let Statement::VariableDeclaration(var) = &script.body.statements[1] {
				assert_eq!(var.var_type, VarType::Bool);
				if let Expression::Primary(Primary::Identifier(id)) = &var.value {
					assert_eq!(id, "false");
				} else {
					panic!("expected identifier false");
				}
			} else {
				panic!("expected second variable declaration");
			}
		}
	}

	#[test]
	fn object_states_array_parse() {
		let src = r#"object OBJ {
    states = {
        { 1, 2, "one.png" },
        { 3, 4, "two.png" },
    };
}"#;
		let ast = parse_ok(src);
		let TopLevel::Object(obj) = &ast[0] else { panic!("expected object") };
		let [Statement::PropertyAssignment(assn)] = obj.body.statements.as_slice() else {
			panic!("one property assignment expected");
		};
		assert_eq!(assn.name, "states");

		let Expression::Primary(Primary::Array(outer)) = &assn.value else {
			panic!("outer value should be array");
		};
		assert_eq!(outer.len(), 2);

		// helper to unwrap literal
		fn lit_num(e: &Expression) -> u32 {
			match e {
				Expression::Primary(Primary::Number(n)) => *n,
				_ => panic!("expected number literal"),
			}
		}
		fn lit_str(e: &Expression) -> &str {
			match e {
				Expression::Primary(Primary::String(s)) => s.as_str(),
				_ => panic!("expected string literal"),
			}
		}

		// first inner array
		let Expression::Primary(Primary::Array(inner0)) = &outer[0] else { panic!() };
		assert_eq!(lit_num(&inner0[0]), 1);
		assert_eq!(lit_num(&inner0[1]), 2);
		assert_eq!(lit_str(&inner0[2]), "one.png");

		// second inner array
		let Expression::Primary(Primary::Array(inner1)) = &outer[1] else { panic!() };
		assert_eq!(lit_num(&inner1[0]), 3);
		assert_eq!(lit_num(&inner1[1]), 4);
		assert_eq!(lit_str(&inner1[2]), "two.png");
	}

	#[test]
	fn interpreter_object_states() {
		let src = r#"object OBJ {
    states = { { 0, 0, "a.png" }, { 0, 0, "b.png" } };
}"#;
		let ast = parse_ok(src);
		let interp = interpreter::Interpreter::new(&ast);
		let states = interp.with_object_by_name("OBJ", |o| o.states.clone()).expect("object missing");
		assert_eq!(states, vec!["a.png".to_string(), "b.png".to_string()]);
	}

	#[test]
	fn object_states_with_empty_strings() {
		let src = r#"object OBJ {
    states = { { 0, 0, "" }, { 0, 0, "visible.png" } };
}"#;
		let ast = parse_ok(src);
		let TopLevel::Object(obj) = &ast[0] else { panic!() };
		let Statement::PropertyAssignment(assn) = &obj.body.statements[0] else {
			panic!()
		};
		assert_eq!(assn.name, "states");

		let Expression::Primary(Primary::Array(outer)) = &assn.value else { panic!() };
		let Expression::Primary(Primary::Array(first)) = &outer[0] else { panic!() };
		let Expression::Primary(Primary::Array(second)) = &outer[1] else { panic!() };

		let empty = match &first[2] {
			Expression::Primary(Primary::String(s)) => s,
			_ => panic!(),
		};
		let vispng = match &second[2] {
			Expression::Primary(Primary::String(s)) => s,
			_ => panic!(),
		};

		assert_eq!(empty, "");
		assert_eq!(vispng, "visible.png");
	}

	#[test]
	fn preprocess_includes_file() {
		let dir = std::env::temp_dir();
		let inc_path = dir.join("inc.sc");
		fs::write(&inc_path, "script Included() {}\n").unwrap();
		let src = format!("#include \"{}\"\nscript Main() {{}}\n", inc_path.display());
		let output = preprocessor::preprocess("main.sc", &src).unwrap();
		assert!(output.contains("script Included()"));
		assert!(output.contains("script Main()"));
	}

	#[test]
	fn object_class_assignment_vs_class_definition() {
		// Test that "class = {ClassName}" in an object is parsed as a ClassAssignment
		// not as a class definition
		let src = r#"object TestObj {
    class = {SomeClass};
    verb test();
}"#;
		let ast = parse_ok(src);
		let TopLevel::Object(obj) = &ast[0] else { panic!() };
		assert_eq!(obj.body.statements.len(), 2);

		// ① the class property
		let Statement::PropertyAssignment(assn) = &obj.body.statements[0] else {
			panic!()
		};
		assert_eq!(assn.name, "class");
		let Expression::Primary(Primary::Array(items)) = &assn.value else { panic!() };
		assert_eq!(items.len(), 1);
		let first = match &items[0] {
			Expression::Primary(Primary::Identifier(id)) => id,
			_ => panic!("identifier expected"),
		};
		assert_eq!(first, "SomeClass");

		// ② the verb stub
		matches!(&obj.body.statements[1], Statement::Verb(v) if v.name == "test");
	}

	#[test]
	fn class_definition_vs_class_assignment() {
		// Test that "class ClassName { ... }" is parsed as a Class definition (TopLevel::Class)
		// while "class = {ClassName};" inside an object is parsed as ClassAssignment
		let src = r#"class BaseClass {
    verb action();
}

object TestObj {
    class = {BaseClass};
}"#;
		let ast = parse_ok(src);

		let TopLevel::Class(cls) = &ast[0] else {
			panic!("Expected first item to be a Class definition");
		};
		assert_eq!(cls.name, "BaseClass");
		assert_eq!(cls.body.statements.len(), 1);
		matches!(&cls.body.statements[0], Statement::Verb(v) if v.name == "action");

		let TopLevel::Object(obj) = &ast[1] else {
			panic!("Expected second item to be an Object definition");
		};
		assert_eq!(obj.id, "TestObj");
		assert_eq!(obj.body.statements.len(), 1);
		let Statement::PropertyAssignment(assn) = &obj.body.statements[0] else {
			panic!()
		};
		let Expression::Primary(Primary::Array(items)) = &assn.value else { panic!() };
		assert_eq!(items.len(), 1);
		let Expression::Primary(Primary::Identifier(cls)) = &items[0] else { panic!() };
		assert_eq!(cls, "BaseClass");
	}

	#[test]
	fn def_block_error_on_unexpected_statement() {
		// Test that def_block (used by object_def and class_def) rejects unexpected statements
		let src = r#"object TestObj {
    if (1) { }
}"#;
		// This should fail because if_stmt is not allowed in def_block
		parse_err(src);
	}

	#[test]
	fn room_block_error_on_unexpected_statement() {
		// Test that room_block rejects unexpected statements
		let src = r#"room TestRoom {
    if (1) { }
}"#;
		// This should fail because if_stmt is not allowed in room_block
		parse_err(src);
	}

	#[test]
	fn class_assignment_multiple_classes() {
		// Test that multiple classes can be assigned to an object
		let src = r#"object TestObj {
    class = {ClassA, ClassB, ClassC};
}"#;
		let ast = parse_ok(src);
		let TopLevel::Object(obj) = &ast[0] else {
			panic!("Expected first item to be an Object definition");
		};
		let Statement::PropertyAssignment(assn) = &obj.body.statements[0] else {
			panic!("Expected property assignment for class");
		};
		assert_eq!(assn.name, "class");

		let Expression::Primary(Primary::Array(classes)) = &assn.value else {
			panic!("Expected class assignment to be an array");
		};
		let idents: Vec<_> = classes
			.iter()
			.map(|e| match e {
				Expression::Primary(Primary::Identifier(s)) => s.as_str(),
				_ => panic!("identifier expected"),
			})
			.collect();

		assert_eq!(idents, ["ClassA", "ClassB", "ClassC"]);
	}

	#[test]
	fn script_with_parameters_parses() {
		let _ast = crate::parse_str(
			r#"script Compute(int x, int y) {
					int z = x + y;
			}"#,
		)
		.expect("parameterised script should parse");
	}
}
