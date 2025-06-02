mod ast;
mod interpreter;
mod preprocessor;

use anyhow::Result;
use ast::{
	Block, Class, Expression, FunctionCall, IfStatement, Object, Primary, PropertyAssignment, PropertyValue, Room, Script, StateEntry, Statement, TopLevel,
	VariableDeclaration, VerbStatement, WhileStatement,
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

fn parse_block(pair: Pair<Rule>) -> Block {
	let mut statements = Vec::new();
	for stmt_pair in pair.into_inner() {
		if let Some(statement) = parse_statement(stmt_pair) {
			statements.push(statement);
		}
	}
	Block { statements }
}

fn parse_def_block(pair: Pair<Rule>) -> Result<Block, String> {
	let mut statements = Vec::new();
	for stmt_pair in pair.into_inner() {
		match stmt_pair.as_rule() {
			Rule::verb_stmt | Rule::states_assign | Rule::class_assign | Rule::prop_assign => {
				if let Some(statement) = parse_statement(stmt_pair) {
					statements.push(statement);
				}
			},
			_ => {
				return Err(format!(
					"Unexpected statement '{:?}' in def_block, expected: verb_stmt, states_assign, class_assign, or prop_assign",
					stmt_pair.as_rule()
				));
			},
		}
	}
	Ok(Block { statements })
}

fn parse_room_block(pair: Pair<Rule>) -> Result<Block, String> {
	let mut statements = Vec::new();
	for stmt_pair in pair.into_inner() {
		match stmt_pair.as_rule() {
			Rule::prop_assign | Rule::verb_stmt | Rule::object_def | Rule::script_def => {
				if let Some(statement) = parse_statement(stmt_pair) {
					statements.push(statement);
				}
			},
			_ => {
				return Err(format!(
					"Unexpected statement '{:?}' in room_block, expected: prop_assign, verb_stmt, object_def, or script_def",
					stmt_pair.as_rule()
				));
			},
		}
	}
	Ok(Block { statements })
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
			let else_block = inner.next().map(parse_block);
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
			Some(Statement::VariableDeclaration(VariableDeclaration { var_type, name, value }))
		},
		Rule::prop_assign => {
			let mut inner = pair.into_inner();
			let name = inner.next()?.as_str().to_string();
			let value_pair = inner.next()?;
			let value = match value_pair.as_rule() {
				Rule::number => PropertyValue::Number(value_pair.as_str().parse().ok()?),
				Rule::string => {
					let raw = value_pair.as_str();
					// remove the leading and trailing quotes (grammar guarantees they exist)
					let inner = &raw[1..raw.len() - 1];
					PropertyValue::String(inner.to_string())
				},
				Rule::identifier => PropertyValue::Identifier(value_pair.as_str().to_string()),
				_ => return None,
			};
			Some(Statement::PropertyAssignment(PropertyAssignment { name, value }))
		},
		Rule::class_assign => {
			let mut inner = pair.into_inner();
			// Extract all class names from class_array
			if let Some(class_array) = inner.next() {
				let mut class_names = Vec::new();
				for class_name_pair in class_array.into_inner() {
					class_names.push(class_name_pair.as_str().to_string());
				}
				Some(Statement::ClassAssignment(class_names))
			} else {
				None
			}
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
		Rule::states_assign => {
			let mut inner = pair.into_inner();
			if let Some(array_pair) = inner.next() {
				let mut states = Vec::new();
				for entry in array_pair.into_inner() {
					let mut ei = entry.into_inner();
					let x: i32 = ei.next()?.as_str().parse().ok()?;
					let y: i32 = ei.next()?.as_str().parse().ok()?;
					let img_pair = ei.next()?;
					let raw = img_pair.as_str();
					let image = raw[1..raw.len() - 1].to_string();
					states.push(StateEntry { x, y, image });
				}
				Some(Statement::States(states))
			} else {
				None
			}
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
		Rule::logical_or => {
			let mut inner = pair.into_inner();
			let mut expr = parse_expression(inner.next().unwrap());

			// Each remaining pair is another logical_and expression to OR with
			for right_pair in inner {
				let right_expr = parse_expression(right_pair);
				expr = Expression::LogicalOr(Box::new(expr), Box::new(right_expr));
			}
			expr
		},
		Rule::logical_and => {
			let mut inner = pair.into_inner();
			let mut expr = parse_expression(inner.next().unwrap());

			// Each remaining pair is another equality expression to AND with
			for right_pair in inner {
				let right_expr = parse_expression(right_pair);
				expr = Expression::LogicalAnd(Box::new(expr), Box::new(right_expr));
			}
			expr
		},
		Rule::equality => {
			let mut inner = pair.into_inner();
			let mut expr = parse_expression(inner.next().unwrap());

			while let Some(op_pair) = inner.next() {
				let right_pair = inner.next().unwrap();
				let op = match op_pair.as_str() {
					"==" => ast::EqualityOp::Equal,
					"!=" => ast::EqualityOp::NotEqual,
					_ => ast::EqualityOp::Equal,
				};
				let right_expr = parse_expression(right_pair);
				expr = Expression::Equality(Box::new(expr), op, Box::new(right_expr));
			}
			expr
		},
		Rule::comparison => {
			let mut inner = pair.into_inner();
			let mut expr = parse_expression(inner.next().unwrap());

			while let Some(op_pair) = inner.next() {
				let right_pair = inner.next().unwrap();
				let op = match op_pair.as_str() {
					"<" => ast::ComparisonOp::Less,
					">" => ast::ComparisonOp::Greater,
					"<=" => ast::ComparisonOp::LessEqual,
					">=" => ast::ComparisonOp::GreaterEqual,
					_ => ast::ComparisonOp::Less,
				};
				let right_expr = parse_expression(right_pair);
				expr = Expression::Comparison(Box::new(expr), op, Box::new(right_expr));
			}
			expr
		},
		Rule::term => {
			let mut inner = pair.into_inner();
			let mut expr = parse_expression(inner.next().unwrap());

			while let Some(op_pair) = inner.next() {
				let right_pair = inner.next().unwrap();
				let op = match op_pair.as_str() {
					"+" => ast::TermOp::Add,
					"-" => ast::TermOp::Subtract,
					_ => ast::TermOp::Add,
				};
				let right_expr = parse_expression(right_pair);
				expr = Expression::Term(Box::new(expr), op, Box::new(right_expr));
			}
			expr
		},
		Rule::factor => {
			let mut inner = pair.into_inner();
			let mut expr = parse_expression(inner.next().unwrap());

			while let Some(op_pair) = inner.next() {
				let right_pair = inner.next().unwrap();
				let op = match op_pair.as_str() {
					"*" => ast::FactorOp::Multiply,
					"/" => ast::FactorOp::Divide,
					_ => ast::FactorOp::Multiply,
				};
				let right_expr = parse_expression(right_pair);
				expr = Expression::Factor(Box::new(expr), op, Box::new(right_expr));
			}
			expr
		},
		Rule::unary => {
			let inner = pair.into_inner();
			let mut ops = Vec::new();
			let mut expr_pair = None;

			for pair in inner {
				match pair.as_str() {
					"!" => ops.push(ast::UnaryOp::Not),
					"-" => ops.push(ast::UnaryOp::Negate),
					_ => expr_pair = Some(pair),
				}
			}

			let mut expr = if let Some(ep) = expr_pair {
				Expression::Primary(parse_primary(ep))
			} else {
				Expression::Primary(Primary::Number(0))
			};

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
		Rule::number => Primary::Number(pair.as_str().parse().unwrap_or(0)),
		Rule::string => {
			let raw = pair.as_str();
			// remove the leading and trailing "   (grammar guarantees they exist)
			let inner = &raw[1..raw.len() - 1];
			Primary::String(inner.to_string())
		},
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

	// Initialize web interface
	if let Err(e) = interpreter.init_web_interface() {
		error!("Failed to initialize web interface: {:?}", e);
		return Err(anyhow::anyhow!("Web interface initialization failed"));
	}

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
	use crate::ast::{EqualityOp, FactorOp, PropertyValue, TermOp};
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

				matches!(obj.body.statements[0], Statement::ClassAssignment(_));

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
				assert_eq!(var.var_type, "string");
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
				assert_eq!(prop.value, PropertyValue::Number(0));
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
				assert_eq!(var.var_type, "int");
				assert!(matches!(var.value, Expression::Primary(Primary::Number(1))));
			} else {
				panic!("expected first variable declaration");
			}

			if let Statement::VariableDeclaration(var) = &script.body.statements[1] {
				assert_eq!(var.var_type, "bool");
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
		if let TopLevel::Object(obj) = &ast[0] {
			assert_eq!(obj.body.statements.len(), 1);
			if let Statement::States(entries) = &obj.body.statements[0] {
				assert_eq!(entries.len(), 2);
				assert_eq!(entries[0].x, 1);
				assert_eq!(entries[0].y, 2);
				assert_eq!(entries[0].image, "one.png");
				assert_eq!(entries[1].x, 3);
				assert_eq!(entries[1].y, 4);
				assert_eq!(entries[1].image, "two.png");
			} else {
				panic!("expected states statement");
			}
		} else {
			panic!("expected object");
		}
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
		assert_eq!(ast.len(), 1);

		match &ast[0] {
			TopLevel::Object(obj) => {
				assert_eq!(obj.id, "TestObj");
				assert_eq!(obj.body.statements.len(), 2);

				// First statement should be a ClassAssignment
				match &obj.body.statements[0] {
					Statement::ClassAssignment(class_names) => {
						assert_eq!(class_names.len(), 1);
						assert_eq!(class_names[0], "SomeClass");
					},
					other => panic!("Expected ClassAssignment, got {:?}", other),
				}

				// Second statement should be a Verb
				match &obj.body.statements[1] {
					Statement::Verb(verb) => {
						assert_eq!(verb.name, "test");
						assert!(verb.body.is_none());
					},
					other => panic!("Expected Verb, got {:?}", other),
				}
			},
			other => panic!("Expected Object, got {:?}", other),
		}
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
		assert_eq!(ast.len(), 2);

		// First should be a class definition
		match &ast[0] {
			TopLevel::Class(class) => {
				assert_eq!(class.name, "BaseClass");
				assert_eq!(class.body.statements.len(), 1);
				match &class.body.statements[0] {
					Statement::Verb(verb) => assert_eq!(verb.name, "action"),
					other => panic!("Expected Verb in class, got {:?}", other),
				}
			},
			other => panic!("Expected Class definition, got {:?}", other),
		}

		// Second should be an object with class assignment
		match &ast[1] {
			TopLevel::Object(obj) => {
				assert_eq!(obj.id, "TestObj");
				match &obj.body.statements[0] {
					Statement::ClassAssignment(class_names) => {
						assert_eq!(class_names.len(), 1);
						assert_eq!(class_names[0], "BaseClass");
					},
					other => panic!("Expected ClassAssignment in object, got {:?}", other),
				}
			},
			other => panic!("Expected Object, got {:?}", other),
		}
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
		assert_eq!(ast.len(), 1);

		match &ast[0] {
			TopLevel::Object(obj) => {
				assert_eq!(obj.id, "TestObj");
				assert_eq!(obj.body.statements.len(), 1);

				match &obj.body.statements[0] {
					Statement::ClassAssignment(class_names) => {
						assert_eq!(class_names.len(), 3);
						assert_eq!(class_names[0], "ClassA");
						assert_eq!(class_names[1], "ClassB");
						assert_eq!(class_names[2], "ClassC");
					},
					other => panic!("Expected ClassAssignment, got {:?}", other),
				}
			},
			other => panic!("Expected Object, got {:?}", other),
		}
	}
}
