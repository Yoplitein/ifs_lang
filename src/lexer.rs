use nbnf::nom::{self, combinator::eof, error::FromExternalError};
use num::Complex;

use crate::AResult;

#[derive(Clone, Debug)]
pub enum Token {
	LParen,
	RParen,
	LBrace,
	RBrace,
	Semicolon,
	Comma,
	Equals,
	Plus,
	Minus,
	Asterisk,
	Slash,
	Caret,

	Var,
	Func,
	For,

	Identifier(String),
	Literal(Literal),
}

#[derive(Clone, Copy, Debug)]
pub enum Literal {
	Real(f64),
	Complex(Complex<f64>),
}

pub fn lex(input: &str) -> AResult<Vec<Token>> {
	let (rest, result) = top
		.parse_complete(input)
		.map_err(|err| anyhow::anyhow!("lexer failed: {err}"))?;
	// should be enforced by `eof` in `top` rule, but just to be safe
	debug_assert!(rest.is_empty());
	Ok(result)
}

nbnf::nbnf!(
	r#"
	// each rule should explicitly define output
	#output <!>

	top<Vec<Token>> = (-whitespace token -whitespace)* -eof;
	whitespace<()> = ([ \t\r\n]*)@<()>;
	token<Token> = (
		"("@<Token::LParen> /
		")"@<Token::RParen> /
		"{"@<Token::LBrace> /
		"}"@<Token::RBrace> /
		";"@<Token::Semicolon> /
		","@<Token::Comma> /
		"="@<Token::Equals> /
		"+"@<Token::Plus> /
		"-"@<Token::Minus> /
		"*"@<Token::Asterisk> /
		"/"@<Token::Slash> /
		"^"@<Token::Caret> /

		"var"@<Token::Var> /
		"func"@<Token::Func> /
		"for"@<Token::For> /

		// literals must be parsed first for complex notation
		literal|<Token::Literal> /
		identifier|<Token::Identifier>
	);

	identifier<String> = -![0-9] ([a-zA-Z0-9]+)|<String::from_iter>;

	literal<Literal> = literal_complex / literal_real;
	scalar<&str> = ~([0-9]+ '.' [0-9]+) / ~([0-9]+);
"#
);

fn literal_real(input: &str) -> nom::IResult<&str, Literal> {
	let (rest, re) = scalar.parse(input)?;
	let re = match re.parse::<f64>() {
		Ok(v) => v,
		Err(err) => {
			return Err(nom::Err::Failure(nom::error::Error::from_external_error(
				input,
				nom::error::ErrorKind::MapRes,
				err,
			)));
		},
	};
	Ok((rest, Literal::Real(re)))
}

fn literal_complex(re_start: &str) -> nom::IResult<&str, Literal> {
	let (rest, re) = scalar.parse(re_start)?;
	let re = match re.parse::<f64>() {
		Ok(v) => v,
		Err(err) => {
			return Err(nom::Err::Failure(nom::error::Error::from_external_error(
				re_start,
				nom::error::ErrorKind::MapRes,
				err,
			)));
		},
	};

	let (rest, _) = nom::bytes::tag("j").parse(rest)?;

	let im_start = rest;
	let (rest, im) = nom::combinator::opt(scalar).parse(rest)?;
	let im = match im.map(|v| v.parse::<f64>()) {
		None => 0.0,
		Some(Ok(v)) => v,
		Some(Err(err)) => {
			return Err(nom::Err::Failure(nom::error::Error::from_external_error(
				im_start,
				nom::error::ErrorKind::MapRes,
				err,
			)));
		},
	};

	let val = Literal::Complex(Complex::new(re, im));
	Ok((rest, val))
}
