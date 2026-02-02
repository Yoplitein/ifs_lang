use std::marker::PhantomData;

use num::Complex;

use super::Compiler;
use crate::{
	AResult,
	lexer::Value,
	parser::{Expr, Module},
};

/// Compiles a function AST back to text, i.e. for reparsing
pub struct TextCompiler<Formatter: ValueFormatter>(PhantomData<Formatter>);

impl<Formatter: ValueFormatter> Default for TextCompiler<Formatter> {
	fn default() -> Self {
		Self(PhantomData)
	}
}

#[derive(Debug)]
pub struct TextOutput {
	pub variables: Vec<String>,
	pub functions: Vec<String>,
}

impl<Formatter: ValueFormatter> Compiler for TextCompiler<Formatter> {
	type NodeOutput = String;
	type Output = TextOutput;

	fn compile_module(&mut self, module: &Module) -> AResult<Self::Output> {
		let variables = module.variables.clone();
		let mut functions = Vec::with_capacity(module.functions.len());
		for function in &module.functions {
			functions.push(self.compile_node(&function.body)?);
		}
		Ok(TextOutput {
			variables,
			functions,
		})
	}

	fn compile_node(&mut self, expr: &Expr) -> AResult<Self::NodeOutput> {
		Ok(match expr {
			Expr::Constant(value) => Formatter::format(*value),
			Expr::Variable(name) => name.clone(),
			Expr::Add(l, r) => format!("({} + {})", self.compile_node(l)?, self.compile_node(r)?),
			Expr::Sub(l, r) => format!("({} - {})", self.compile_node(l)?, self.compile_node(r)?),
			Expr::Mul(l, r) => format!("({} * {})", self.compile_node(l)?, self.compile_node(r)?),
			Expr::Div(l, r) => format!("({} / {})", self.compile_node(l)?, self.compile_node(r)?),
			// MAYBE: abstract operator lowering
			Expr::Pow(l, r) => format!("({} ^ {})", self.compile_node(l)?, self.compile_node(r)?),
			Expr::Neg(expr) => format!("(-{})", self.compile_node(expr)?),
			Expr::Call { function, args } => {
				let args: AResult<Vec<_>> =
					args.into_iter().map(|v| self.compile_node(v)).collect();
				let args = args?.join(", ");
				format!("{function}({args})")
			},
		})
	}
}

pub trait ValueFormatter {
	fn format(value: Value) -> String;
}

pub struct FormatStandard;

impl ValueFormatter for FormatStandard {
	fn format(value: Value) -> String {
		match value {
			Value::Real(v) => format!("{v}"),
			Value::Complex(Complex { re, im }) => format!("{re}+{im}i"),
		}
	}
}
