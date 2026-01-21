use std::{collections::HashMap, iter::Enumerate, ops::Range, slice::Iter};

use nbnf::nom::{self, combinator::eof, error::FromExternalError};
use strum::IntoDiscriminant;

use crate::{AResult, lexer::{Token, TokenTy, Value}};

#[derive(Debug, Default)]
pub struct Module {
	pub variables: Vec<String>,
	pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct Function {
	pub constants: HashMap<String, Value>,
	pub body: Expr,
}

impl Function {
	fn new(body: Expr) -> Self {
		Self {
			constants: HashMap::new(),
			body,
		}
	}
}

#[derive(Clone, Debug)]
pub enum Expr {
	Value(Value),
	Variable(String),

	Add(Box<Self>, Box<Self>),
	Sub(Box<Self>, Box<Self>),
	Mul(Box<Self>, Box<Self>),
	Div(Box<Self>, Box<Self>),
	Pow(Box<Self>, Box<Self>),

	Call {
		function: String,
		args: Vec<Self>,
	}
}

pub fn parse(input: &[Token]) -> AResult<Module> {
	let input = Tokens(input);
	let (rest, result) = top
		.parse_complete(input)
		.map_err(|err| anyhow::anyhow!("parser failed: {err}"))?;
	debug_assert!(rest.0.is_empty());
	Ok(result)
}

#[derive(Debug)]
enum Top {
	VarDecl(String),
	Func(Expr),
	ForLoop {
		variable: String,
		range: Range<Value>,
		body: Vec<Top>,
	}
}

nbnf::nbnf!(r#"
	#input <Tokens>
	#output <!>

	top<Module> =
		(
			var_decl /
			func /
			for_loop
		)+|<unwrap_top>
		-eof;
	
	var_decl<Top> =
		-<token(TokenTy::Var)>
		<token(TokenTy::Identifier)>|<map_var_decl>
		-<token(TokenTy::Semicolon)>;
	
	for_loop<Top> = (
		-<token(TokenTy::For)>
		<token(TokenTy::Identifier)>
		-<token(TokenTy::Equals)>
		<token(TokenTy::Literal)>
		-<token(TokenTy::Comma)>
		// TODO: optional step value
		<token(TokenTy::Literal)>
		-<token(TokenTy::LBrace)>
		func+
		-<token(TokenTy::RBrace)>
	)|<map_for_loop>;
	
	func<Top> =
		-<token(TokenTy::Func)>
		expr|<Top::Func>
		-<token(TokenTy::Semicolon)>;
	
	expr<Expr> =
		<token(TokenTy::Literal)>|<map_literal>;
"#);

fn token(ty: TokenTy) -> impl Fn(Tokens) -> nom::IResult<Tokens, &Token> {
	move |input: Tokens| -> nom::IResult<Tokens, &Token> {
		let (rest, token) = nom::bytes::complete::take(1usize).parse(input)?;
		let token = &token.0[0];
		if token.discriminant() != ty {
			return Err(nom::Err::Error(nom::error::Error::from_external_error(
				input,
				nom::error::ErrorKind::MapRes,
				format!("expected token {ty:?} but got {:?}", token.discriminant()),
			)));
		}
		Ok((rest, token))
	}
}

fn unwrap_top(nodes: Vec<Top>) -> Module {
	let mut module = Module::default();
	for node in nodes {
		match node {
			Top::VarDecl(name) => module.variables.push(name),
			Top::Func(body) => module.functions.push(Function::new(body)),
			Top::ForLoop { variable, range, body: inner } => {
				let Value::Real(mut value) = range.start else {
					// FIXME: propagate via result
					panic!("for loop start cannot be complex")
				};
				let Value::Real(end) = range.end else {
					panic!("for loop end cannot be complex")
				};
				// TODO: user-specified step value
				let step = 1.0;
				while value < end {
					let constants = HashMap::from_iter([(
						variable.clone(),
						Value::Real(value),
					)]);
					for body in &inner {
						let constants = constants.clone();
						let Top::Func(body) = body else {
							panic!("FIXME: permit nested loops etc")
						};
						// TODO: slap in an Arc?
						let body = body.clone();
						module.functions.push(Function {
							constants,
							body,
						})
					}
					value += step;
				}
			},
		}
	}
	module
}

fn map_var_decl(token: &Token) -> Top {
	let Token::Identifier(name) = token else {
		unreachable!("parsed identifier but getting different token")
	};
	Top::VarDecl(name.clone())
}

fn map_for_loop((variable, start, end, body): (&Token, &Token, &Token, Vec<Top>)) -> Top {
	let Token::Identifier(variable) = variable else {
		unreachable!("parsed identifier but getting different token")
	};
	let variable = variable.clone();
	let Token::Literal(start) = start else {
		unreachable!("parsed literal but getting different token")
	};
	let Token::Literal(end) = end else {
		unreachable!("parsed literal but getting different token")
	};
	let range = *start .. *end;
	Top::ForLoop {
		variable,
		range,
		body,
	}
}

fn map_literal(token: &Token) -> Expr {
	let &Token::Literal(value) = token else {
		unreachable!("parsed literal but getting different token")
	};
	Expr::Value(value)
}

#[derive(Clone, Copy, Debug)]
struct Tokens<'a>(&'a [Token]);

impl<'a> nom::Input for Tokens<'a> {
	type Item = &'a Token;
	type Iter = Iter<'a, Token>;
	type IterIndices = Enumerate<Self::Iter>;

	fn input_len(&self) -> usize {
		self.0.len()
	}

	fn take(&self, index: usize) -> Self {
		Self(&self.0[.. index])
	}

	fn take_from(&self, index: usize) -> Self {
		Self(&self.0[index ..])
	}

	fn take_split(&self, index: usize) -> (Self, Self) {
		let (l, r) = self.0.split_at(index);
		(Self(l), Self(r))
	}

	fn position<P>(&self, predicate: P) -> Option<usize>
	  where
		P: Fn(Self::Item) -> bool {
		self.0.iter().position(predicate)
	}

	fn iter_elements(&self) -> Self::Iter {
		self.0.iter()
	}

	fn iter_indices(&self) -> Self::IterIndices {
		self.0.iter().enumerate()
	}

	fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
		if self.0.len() >= count {
			Ok(count)
		} else {
			Err(nom::Needed::new(count - self.0.len()))
		}
	}
}

#[test]
fn ree() {
	let inp = r#"
		var a;
		var b;
		func 1;
		func 2;
		for x = 0, 3 {
			func 3;
		}
	"#;
	let inp = crate::lexer::lex(inp).unwrap();
	dbg!(&inp);
	let parsed = parse(&inp).unwrap();
	// let parsed = for_loop.parse(Tokens(&inp)).unwrap();
	dbg!(parsed);

	panic!("skree");
}
