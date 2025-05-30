mod ast;
mod interpreter;
mod preprocessor;

use anyhow::{Context, Result};
use wasm_bindgen::prelude::*;
use web_sys::{window, Response};
use wasm_bindgen_futures::{spawn_local, JsFuture};
use ast::{
	Block, Class, Expression, FunctionCall, IfStatement, Object, Primary, PropertyAssignment, PropertyValue, Script, ScriptName, StateStatement, Statement,
	TopLevel, VariableDeclaration, VerbStatement, WhileStatement,
};
use pest::Parser;
use preprocessor::preprocess;
use std::{env, path::Path};

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
		Rule::class_stmt => {
			let mut inner = pair.into_inner();
			let name = inner.next()?.as_str().to_string();
			Some(Statement::ClassDeclaration(name))
		},
		Rule::verb_stmt => {
			let mut inner = pair.into_inner();
			let name = inner.next()?.as_str().to_string();
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
				Rule::string => PropertyValue::String(value_pair.as_str().to_string()),
				Rule::identifier => PropertyValue::Identifier(value_pair.as_str().to_string()),
				_ => return None,
			};
			Some(Statement::PropertyAssignment(PropertyAssignment { name, value }))
		},
		Rule::state_stmt => {
			let mut inner = pair.into_inner();
			let number = inner.next()?.as_str().parse().ok()?;
			let mut assignments = Vec::new();
			while let Some(pair) = inner.next() {
				if pair.as_rule() == Rule::identifier {
					let key = pair.as_str().to_string();
					if let Some(value_pair) = inner.next() {
						let value = parse_primary(value_pair);
						assignments.push((key, value));
					}
				}
			}
			Some(Statement::State(StateStatement { number, assignments }))
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
		_ => Primary::Number(0), // Fallback
	}
}

pub fn parse_str(input: &str) -> Result<Vec<TopLevel>> {
	let pairs = ScummParser::parse(Rule::file, input)?;
	let mut ast_nodes = Vec::new();

	for pair in pairs {
		if pair.as_rule() == Rule::file {
			for inner_pair in pair.into_inner() {
				match inner_pair.as_rule() {
					Rule::directive => {
						let mut inner = inner_pair.into_inner();
						let name = inner.next().unwrap().as_str().to_string();
						let value = inner.next().map(|p| p.as_str().trim().to_string()).unwrap_or_default();
						ast_nodes.push(TopLevel::Directive(name, value));
					},
					Rule::item => {
						for item_pair in inner_pair.into_inner() {
							match item_pair.as_rule() {
								Rule::script_def => {
									let mut script_inner = item_pair.into_inner();
									let name_pair = script_inner.next().unwrap();
									let name = match name_pair.as_str().parse::<u32>() {
										Ok(num) => ScriptName::Number(num),
										Err(_) => ScriptName::Identifier(name_pair.as_str().to_string()),
									};

									let body = if let Some(block_pair) = script_inner.next() {
										parse_block(block_pair)
									} else {
										Block { statements: Vec::new() }
									};

									ast_nodes.push(TopLevel::Script(Script { name, body }));
								},
								Rule::object_def => {
									let mut object_inner = item_pair.into_inner();
									let id = object_inner.next().unwrap().as_str().to_string();
									let name = object_inner.next().unwrap().as_str().to_string();
									let body = if let Some(block_pair) = object_inner.next() {
										parse_block(block_pair)
									} else {
										Block { statements: Vec::new() }
									};

									ast_nodes.push(TopLevel::Object(Object { id, name, body }));
								},
								Rule::class_def => {
									let mut class_inner = item_pair.into_inner();
									let name = class_inner.next().unwrap().as_str().to_string();
									let body = if let Some(block_pair) = class_inner.next() {
										parse_block(block_pair)
									} else {
										Block { statements: Vec::new() }
									};

									ast_nodes.push(TopLevel::Class(Class { name, body }));
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
    let resp_value = JsFuture::from(
        window().unwrap().fetch_with_str("/example.sc")
    )
    .await?;

    let resp: Response = resp_value.dyn_into()?;
    let text = JsFuture::from(resp.text()?).await?;
    Ok(text.as_string().unwrap())
}


#[wasm_bindgen]
pub fn start() {
	web_sys::console::log_1(&JsValue::from_str("Starting Scumm Script Parser..."));
	spawn_local(async {
		match load_script_raw().await {
			Ok(source) => {
				handle_script(source.as_bytes())
					.map_err(|e| web_sys::console::error_1(&JsValue::from_str(&e.to_string())))
					.unwrap_or(());
			}
			Err(err) => web_sys::console::error_1(&err),
		}
	});
}


fn handle_script(data: &[u8]) -> Result<(), anyhow::Error> {
	// Phase 1: preprocess includes.
	let flattened = preprocess("main.sc", &String::from_utf8_lossy(data))?;
	println!("Preprocessed content length: {} chars", flattened.len());

	// Phase 2: parse.
	let pairs = match ScummParser::parse(Rule::file, &flattened) {
		Ok(pairs) => pairs,
		Err(e) => {
			eprintln!("Parse error: {}", e);
			eprintln!("Preprocessed content preview:");
			let lines: Vec<&str> = flattened.lines().collect();
			for (i, line) in lines.iter().enumerate().take(50) {
				eprintln!("{:3}: {}", i + 1, line);
			}
			return Err(anyhow::anyhow!(e));
		},
	};

	// Phase 3: build AST
	let mut ast_nodes = Vec::new();
	for pair in pairs {
		if pair.as_rule() == Rule::file {
			for inner_pair in pair.into_inner() {
				match inner_pair.as_rule() {
					Rule::directive => {
						let mut inner = inner_pair.into_inner();
						let name = inner.next().unwrap().as_str().to_string();
						let value = inner.next().map(|p| p.as_str().trim().to_string()).unwrap_or_default();
						ast_nodes.push(TopLevel::Directive(name, value));
					},
					Rule::item => {
						// For now, just add placeholder items
						for item_pair in inner_pair.into_inner() {
							match item_pair.as_rule() {
								Rule::script_def => {
									// Parse script definition
									let mut script_inner = item_pair.into_inner();
									let name_pair = script_inner.next().unwrap();
									let name = match name_pair.as_str().parse::<u32>() {
										Ok(num) => ScriptName::Number(num),
										Err(_) => ScriptName::Identifier(name_pair.as_str().to_string()),
									};

									// Parse the script body
									let body = if let Some(block_pair) = script_inner.next() {
										parse_block(block_pair)
									} else {
										Block { statements: Vec::new() }
									};

									let script = Script { name, body };
									ast_nodes.push(TopLevel::Script(script));
								},
								Rule::object_def => {
									// Parse object definition
									let mut object_inner = item_pair.into_inner();
									let id = object_inner.next().unwrap().as_str().to_string();
									let name = object_inner.next().unwrap().as_str().to_string();
									let body = if let Some(block_pair) = object_inner.next() {
										parse_block(block_pair)
									} else {
										Block { statements: Vec::new() }
									};

									let object = Object { id, name, body };
									ast_nodes.push(TopLevel::Object(object));
								},
								Rule::class_def => {
									// Parse class definition
									let mut class_inner = item_pair.into_inner();
									let name = class_inner.next().unwrap().as_str().to_string();
									let body = if let Some(block_pair) = class_inner.next() {
										parse_block(block_pair)
									} else {
										Block { statements: Vec::new() }
									};

									let class = Class { name, body };
									ast_nodes.push(TopLevel::Class(class));
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

	//println!("Parsed successfully! AST nodes: {:#?}", ast_nodes);
	web_sys::console::log_1(&JsValue::from_str(&format!("Parsed successfully! AST nodes: {:?}", ast_nodes)));

	//interpreter::run_scripts(&ast_nodes);

	Ok(())
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


#[cfg(test)]
mod tests {
	use super::*;
	use crate::ast::{EqualityOp, FactorOp, PropertyValue, TermOp};

	fn parse_ok(src: &str) -> Vec<TopLevel> {
		parse_str(src).expect("parse failed")
	}

	fn parse_err(src: &str) {
		assert!(parse_str(src).is_err());
	}

	#[test]
	fn directive_parsing() {
		let ast = parse_ok("#define FOO 1\n");
		assert_eq!(ast, vec![TopLevel::Directive("define".to_string(), "FOO 1".to_string())]);
	}

	#[test]
	fn script_name_identifier() {
		let ast = parse_ok("script ROOM { }\n");
		assert_eq!(
			ast,
			vec![TopLevel::Script(Script {
				name: ScriptName::Identifier("ROOM".to_string()),
				body: Block { statements: vec![] },
			})]
		);
	}

	#[test]
	fn script_name_number() {
		let ast = parse_ok("script 5 { }\n");
		assert_eq!(
			ast,
			vec![TopLevel::Script(Script {
				name: ScriptName::Number(5),
				body: Block { statements: vec![] },
			})]
		);
	}

	#[test]
	fn object_with_verbs() {
		let src = r#"object OBJ "Name" {
    class Cls;
    verb vOne;
    verb vTwo {
        call();
    }
}"#;
		let ast = parse_ok(src);

		assert_eq!(ast.len(), 1);
		match &ast[0] {
			TopLevel::Object(obj) => {
				assert_eq!(obj.id, "OBJ");
				assert_eq!(obj.name, "\"Name\"");
				assert_eq!(obj.body.statements.len(), 3);

				matches!(obj.body.statements[0], Statement::ClassDeclaration(_));

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
		let src = r#"script S {
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
		let src = r#"script S {
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
		let src = r#"script S {
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
	fn state_statement() {
		let src = r#"object OBJ "o" {
    state 0 open=1;
}"#;
		let ast = parse_ok(src);
		if let TopLevel::Object(obj) = &ast[0] {
			if let Statement::State(state) = &obj.body.statements[0] {
				assert_eq!(state.number, 0);
				assert_eq!(state.assignments.len(), 1);
				assert_eq!(state.assignments[0].0, "open");
				assert_eq!(state.assignments[0].1, Primary::Number(1));
			} else {
				panic!("expected state statement");
			}
		}
	}

	#[test]
	fn class_definition() {
		let src = r#"class C {
    verb vRun { run(); }
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
		let src = r#"script S {
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
		let src = r#"object O "o" {
    initialRoom 0;
}"#;
		let ast = parse_ok(src);
		if let TopLevel::Object(obj) = &ast[0] {
			if let Statement::PropertyAssignment(prop) = &obj.body.statements[0] {
				assert_eq!(prop.name, "initialRoom");
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
		let src = r#"script S {
    a = 3 + 4 * 5;
}"#;
		let ast = parse_ok(src);
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
		let src = r#"script S {
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
	fn multi_state_assignments() {
		let src = r#"object O "o" {
    state 1 a=1 b=2;
}"#;
		let ast = parse_ok(src);
		if let TopLevel::Object(obj) = &ast[0] {
			if let Statement::State(state) = &obj.body.statements[0] {
				assert_eq!(state.number, 1);
				assert_eq!(state.assignments.len(), 2);
				assert_eq!(state.assignments[0].0, "a");
				assert_eq!(state.assignments[0].1, Primary::Number(1));
				assert_eq!(state.assignments[1].0, "b");
				assert_eq!(state.assignments[1].1, Primary::Number(2));
			} else {
				panic!("expected state statement");
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
		let src = "script S { string a = 1 }";
		parse_err(src);
	}

	#[test]
	fn invalid_object_syntax() {
		let src = "object O { }";
		parse_err(src);
	}

	#[test]
	fn invalid_unclosed_block() {
		let src = "script S {";
		parse_err(src);
	}
}
