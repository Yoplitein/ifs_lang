use std::{iter::Enumerate, slice::Iter};

use nbnf::nom::{self, combinator::eof, error::FromExternalError};
use strum::IntoDiscriminant;

use crate::{AResult, lexer::{Token, TokenTy, Value}};

#[derive(Debug)]
pub struct Module {
	pub variables: Vec<String>,
	pub functions: Vec<Expr>,
}

#[derive(Debug)]
pub enum Expr {
	Literal(Value),
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

nbnf::nbnf!(r#"
	#input <Tokens>
	#output <!>

	top<Module> = -<token(TokenTy::Var)> <token(TokenTy::Identifier)>|<map_mod> -<token(TokenTy::Semicolon)> -eof;
"#);

fn token(ty: TokenTy) -> impl Fn(Tokens) -> nom::IResult<Tokens, &Token> {
	move |input: Tokens| -> nom::IResult<Tokens, &Token> {
		let (rest, token) = nom::bytes::complete::take(1usize).parse(input)?;
		let token = &token.0[0];
		if token.discriminant() != ty {
			return Err(nom::Err::Failure(nom::error::Error::from_external_error(
				input,
				nom::error::ErrorKind::MapRes,
				format!("expected token {ty:?} but got {:?}", token.discriminant()),
			)));
		}
		Ok((rest, token))
	}
}

fn map_mod(ident: &Token) -> Module {
	let Token::Identifier(v) = ident else { unreachable!() };
	Module {
		variables: vec![v.clone()],
		functions: vec![],
	}
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
		var a
	"#;
	let inp = crate::lexer::lex(inp).unwrap();
	dbg!(&inp);
	let parsed = parse(&inp).unwrap();
	dbg!(parsed);

	panic!("skree");
}
