mod ast;
mod preprocessor;

use anyhow::{Context, Result};
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
			if let Some(expr_pair) = inner.next() {
				Some(Statement::Expression(parse_expression(expr_pair)))
			} else {
				None
			}
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
			Some(Statement::VerbStatement(VerbStatement { name, body }))
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
			Some(Statement::StateStatement(StateStatement { number, assignments }))
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

			// Each remaining pair is another comparison to compare with using ==
			// The equality rule only supports == for now (we'd need to modify grammar for !=)
			for right_pair in inner {
				let right_expr = parse_expression(right_pair);
				expr = Expression::Equality(Box::new(expr), ast::EqualityOp::Equal, Box::new(right_expr));
			}
			expr
		},
		Rule::comparison => {
			let mut inner = pair.into_inner();
			let mut expr = parse_expression(inner.next().unwrap());

			// Process remaining pairs in groups: operator, operand, operator, operand, ...
			let remaining: Vec<_> = inner.collect();
			let mut i = 0;
			while i + 1 < remaining.len() {
				let op_str = remaining[i].as_str();
				let right_expr = parse_expression(remaining[i + 1].clone());

				let op = match op_str {
					"<" => ast::ComparisonOp::Less,
					">" => ast::ComparisonOp::Greater,
					"<=" => ast::ComparisonOp::LessEqual,
					">=" => ast::ComparisonOp::GreaterEqual,
					_ => ast::ComparisonOp::Less, // fallback
				};

				expr = Expression::Comparison(Box::new(expr), op, Box::new(right_expr));
				i += 2;
			}
			expr
		},
		Rule::term => {
			let mut inner = pair.into_inner();
			let mut expr = parse_expression(inner.next().unwrap());

			// Process remaining pairs in groups: operator, operand, operator, operand, ...
			let remaining: Vec<_> = inner.collect();
			let mut i = 0;
			while i + 1 < remaining.len() {
				let op_str = remaining[i].as_str();
				let right_expr = parse_expression(remaining[i + 1].clone());

				let op = match op_str {
					"+" => ast::TermOp::Add,
					"-" => ast::TermOp::Subtract,
					_ => ast::TermOp::Add, // fallback
				};

				expr = Expression::Term(Box::new(expr), op, Box::new(right_expr));
				i += 2;
			}
			expr
		},
		Rule::factor => {
			let mut inner = pair.into_inner();
			let mut expr = parse_expression(inner.next().unwrap());

			// Process remaining pairs in groups: operator, operand, operator, operand, ...
			let remaining: Vec<_> = inner.collect();
			let mut i = 0;
			while i + 1 < remaining.len() {
				let op_str = remaining[i].as_str();
				let right_expr = parse_expression(remaining[i + 1].clone());

				let op = match op_str {
					"*" => ast::FactorOp::Multiply,
					"/" => ast::FactorOp::Divide,
					_ => ast::FactorOp::Multiply, // fallback
				};

				expr = Expression::Factor(Box::new(expr), op, Box::new(right_expr));
				i += 2;
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
		Rule::string => Primary::String(pair.as_str().to_string()),
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

fn main() -> Result<()> {
	let filename = env::args().nth(1).context("Usage: scummc_compiler <file.sc>")?;
	let entry = Path::new(&filename);

	// Phase 1: preprocess includes.
	let flattened = preprocess(entry)?;
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

	println!("Parsed successfully! AST nodes: {:#?}", ast_nodes);
	Ok(())
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::ast::EqualityOp;

	fn parse_ok(src: &str) -> Vec<TopLevel> {
		parse_str(src).expect("parse failed")
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

				if let Statement::VerbStatement(ref verb) = obj.body.statements[1] {
					assert_eq!(verb.name, "vOne");
					assert!(verb.body.is_none());
				} else {
					panic!("expected verb statement");
				}

				if let Statement::VerbStatement(ref verb) = obj.body.statements[2] {
					assert_eq!(verb.name, "vTwo");
					let body = verb.body.as_ref().unwrap();
					assert_eq!(body.statements.len(), 1);
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
				if let Expression::Equality(_, op, _) = &stmt.condition {
					assert_eq!(*op, EqualityOp::Equal);
				} else {
					panic!("expected equality condition");
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
			if let Statement::StateStatement(state) = &obj.body.statements[0] {
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
					// left should be a function call
					if let Expression::Primary(Primary::FunctionCall(fc)) = &**left {
						assert_eq!(fc.name, "a");
					} else {
						panic!("expected function call");
					}
					if let Expression::Equality(_, EqualityOp::Equal, _) = &**right {
					} else {
						panic!("expected equality");
					}
				} else {
					panic!("expected logical and");
				}
			}
		}
	}
}
