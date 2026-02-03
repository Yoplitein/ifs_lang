use std::{collections::HashMap, iter::Enumerate, ops::Range, slice::Iter};

use anyhow::bail;
use nbnf::nom::{self, combinator::eof, error::FromExternalError, multi::separated_list1};
use strum::{EnumDiscriminants, IntoDiscriminant};

use crate::{
	AResult,
	lexer::{Token, TokenTy, Value},
};

#[derive(Debug, Default)]
pub struct Module {
	pub globals: Vec<String>,
	pub arguments: Vec<String>,
	pub functions: Vec<Function>,
}

pub type VariableMap = HashMap<String, Value>;

#[derive(Debug)]
pub struct Function {
	pub constants: VariableMap,
	pub body: Expr,
}

#[derive(Clone, Debug, EnumDiscriminants)]
#[strum_discriminants(name(ExprTy))]
pub enum Expr {
	Constant(Value),
	Variable(String),

	Add(Box<Self>, Box<Self>),
	Sub(Box<Self>, Box<Self>),
	Mul(Box<Self>, Box<Self>),
	Div(Box<Self>, Box<Self>),
	Pow(Box<Self>, Box<Self>),
	Neg(Box<Self>),

	Call { function: String, args: Vec<Self> },
}

pub fn parse(input: &[Token]) -> AResult<Module> {
	let input = Tokens(input);
	let (rest, nodes) = top
		.parse_complete(input)
		.map_err(|err| anyhow::anyhow!("parser failed: {err}"))?;
	debug_assert!(rest.0.is_empty());

	let mut module = Module::default();
	let mut constants = HashMap::new();
	for node in nodes {
		node.fold(&mut module, &mut constants)?;
	}

	for func in &module.functions {
		for variable in &module.globals {
			if func.constants.contains_key(variable) {
				bail!(
					"invalid module: symbol `{variable}` is defined as both a variable and a \
					 constant"
				);
			}
		}
	}

	Ok(module)
}

#[derive(Debug)]
enum Top {
	VarDecl(Vec<String>),
	ArgDecl(Vec<String>),
	ConstDecl {
		variable: String,
		value: Value,
	},
	Func(Expr),
	ForLoop {
		variable: String,
		range: Range<Value>,
		body: Vec<Top>,
	},
}

impl Top {
	fn fold(&self, module: &mut Module, constants: &mut VariableMap) -> AResult<()> {
		match self {
			Top::VarDecl(names) => module.globals.extend(names.clone()),
			Top::ArgDecl(names) => module.arguments.extend(names.clone()),
			Top::ConstDecl { variable, value } => {
				if constants.contains_key(variable) {
					bail!("redefinition of constant `{}`", variable);
				}
				constants.insert(variable.clone(), value.clone());
			},
			Top::Func(expr) => module.functions.push(Function {
				constants: constants.clone(),
				body: expr.clone(),
			}),
			Top::ForLoop {
				variable,
				range,
				body,
			} => {
				let (Value::Real(mut value), Value::Real(end)) = (range.start, range.end) else {
					bail!("for loop start/end cannot be complex")
				};
				// TODO: user-specified step value
				let step = 1.0;
				while value < end {
					if constants.contains_key(variable) {
						bail!("redefinition of constant `{}`", variable);
					}
					constants.insert(variable.clone(), Value::Real(value));
					for node in body {
						node.fold(module, constants)?;
					}
					constants.remove(variable);
					value += step;
				}
			},
		}
		Ok(())
	}
}

#[rustfmt::skip]
nbnf::nbnf!(r#"
	#input <Tokens>
	#output <!>

	top<Vec<Top>> =
		(
			var_or_arg_decl /
			const_decl /
			func /
			for_loop
		)+
		-eof;

	var_or_arg_decl<Top> =
		(
			(
				<token(TokenTy::Var)> /
				<token(TokenTy::Arg)>
			)
			<separated_list1(token(TokenTy::Comma), token(TokenTy::Identifier))>
		)|<map_var_or_arg_decl>
		-<token(TokenTy::Semicolon)>;

	const_decl<Top> = (
		-<token(TokenTy::Const)>
		<token(TokenTy::Identifier)>
		-<token(TokenTy::Equals)>
		<token(TokenTy::Literal)>
		-<token(TokenTy::Semicolon)>
	)|<map_const_decl>;

	for_loop<Top> = (
		-<token(TokenTy::For)>
		<token(TokenTy::Identifier)>
		-<token(TokenTy::Equals)>
		<token(TokenTy::Literal)>
		-<token(TokenTy::Comma)>
		// TODO: optional step value
		<token(TokenTy::Literal)>
		-<token(TokenTy::LBrace)>
		(func / for_loop)+
		-<token(TokenTy::RBrace)>
	)|<map_for_loop>;

	func<Top> =
		-<token(TokenTy::Func)>
		expr|<Top::Func>
		-<token(TokenTy::Semicolon)>;

	expr<Expr> = expr_p0;

	expr_p0<Expr> = (
		expr_p1
		(
			(
				<token(TokenTy::Plus)> /
				<token(TokenTy::Minus)>
			)
			expr_p1
		)*
	)|<fold_expr>;

	expr_p1<Expr> = (
		expr_p2
		(
			(
				<token(TokenTy::Asterisk)> /
				<token(TokenTy::Slash)>
			)
			expr_p2
		)*
	)|<fold_expr>;

	expr_p2<Expr> = (
		expr_p3
		(
			<token(TokenTy::Caret)>
			expr_p2
		)*
	)|<fold_expr>;

	expr_p3<Expr> = (
		(
			-<token(TokenTy::Minus)>
			expr_p3|<|v| Expr::Neg(v.into())>
		) / expr_p4
	);

	expr_p4<Expr> =
		expr_call /
		expr_literal /
		expr_variable /
		(
			-<token(TokenTy::LParen)>
			expr_p0
			-<token(TokenTy::RParen)>
		);

	expr_call<Expr> = (
		<token(TokenTy::Identifier)>
		-<token(TokenTy::LParen)>
		<separated_list1(token(TokenTy::Comma), expr)>
		-<token(TokenTy::RParen)>
	)|<map_expr_call>;
	expr_literal<Expr> =
		<token(TokenTy::Literal)>|<map_expr_literal>;
	expr_variable<Expr> =
		<token(TokenTy::Identifier)>|<map_expr_variable>;
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

fn map_var_or_arg_decl((decl_ty, names): (&Token, Vec<&Token>)) -> Top {
	let names = names
		.into_iter()
		.map(|name| {
			let Token::Identifier(name) = name else {
				unreachable!("parsed identifier but getting different token")
			};
			name.clone()
		})
		.collect();
	match decl_ty {
		Token::Var => Top::VarDecl(names),
		Token::Arg => Top::ArgDecl(names),
		_ => unreachable!("parsed Var/Arg but getting different token"),
	}
}

fn map_const_decl((variable, value): (&Token, &Token)) -> Top {
	let Token::Identifier(variable) = variable.clone() else {
		unreachable!("parsed identifier but getting different token")
	};
	let Token::Literal(value) = *value else {
		unreachable!("parsed literal but getting different token")
	};
	Top::ConstDecl { variable, value }
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

fn fold_expr((lhs, rhs): (Expr, Vec<(&Token, Expr)>)) -> Expr {
	let mut top = lhs;
	for (op, rhs) in rhs {
		let lhs = top.into();
		let rhs = rhs.into();
		top = match op {
			Token::Plus => Expr::Add(lhs, rhs),
			Token::Minus => Expr::Sub(lhs, rhs),
			Token::Asterisk => Expr::Mul(lhs, rhs),
			Token::Slash => Expr::Div(lhs, rhs),
			Token::Caret => Expr::Pow(lhs, rhs),
			_ => unreachable!("parsed operator token but trying to fold expr with different token"),
		};
	}
	top
}

fn map_expr_call((func, args): (&Token, Vec<Expr>)) -> Expr {
	let Token::Identifier(function) = func else {
		unreachable!("parsed identifier but getting different token")
	};
	let function = function.clone();
	Expr::Call { function, args }
}

fn map_expr_literal(token: &Token) -> Expr {
	let &Token::Literal(value) = token else {
		unreachable!("parsed literal but getting different token")
	};
	Expr::Constant(value)
}

fn map_expr_variable(token: &Token) -> Expr {
	let Token::Identifier(name) = token else {
		unreachable!("parsed identifier but getting different token")
	};
	Expr::Variable(name.clone())
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
		P: Fn(Self::Item) -> bool,
	{
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
