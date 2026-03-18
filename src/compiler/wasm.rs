use std::{
	collections::{HashMap, HashSet},
	fmt::Write,
};

use anyhow::{Ok, bail, ensure};
use num::Complex;

use super::Compiler;
use crate::{
	AResult,
	lexer::Value,
	parser::{ArgumentSet, Expr, Module},
};

#[derive(Default)]
pub struct WasmCompiler {
	globals: HashSet<String>,
	arguments: ArgumentSet,
	current_function: String,
	// name => arity
	host_functions: HashMap<String, usize>,
}

pub struct WasmOutput {
	pub globals: HashSet<String>,
	pub arguments: ArgumentSet,
	pub num_ifs_functions: usize,
	pub wat_module: String,
}

impl Compiler for WasmCompiler {
	type NodeOutput = ();
	type Output = WasmOutput;

	fn compile_module(mut self, module: &Module) -> AResult<Self::Output> {
		self.globals.clone_from(&module.globals);
		self.arguments.clone_from(&module.arguments);

		let mut globals = Vec::with_capacity(self.globals.len());
		for name in &self.globals {
			globals.push(format!(
				"(global ${name} (export \"{name}\") (mut v128) (v128.const f64x2 0.0 0.0))\n"
			))
		}

		let args = {
			let mut ordered = Vec::from_iter(self.arguments.names());
			ordered.sort_by_key(|name| self.arguments.get(name));
			let mut args = String::new();
			for name in ordered {
				if !args.is_empty() {
					args.push(' ');
				}
				args.write_fmt(format_args!("(param ${name} v128)"))?;
			}
			args
		};
		let mut functions = Vec::with_capacity(module.functions.len());
		for (index, function) in module.functions.iter().enumerate() {
			self.compile_node(&function.body)?;
			let instructions = std::mem::take(&mut self.current_function);
			functions.push(format!(
				"(func (export \"f{index}\") {args} (result v128)\n{instructions})\n"
			));
		}

		let mut imports = Vec::with_capacity(self.host_functions.len());
		let mut arg_decls = HashMap::new();
		for (name, &arity) in &self.host_functions {
			let args = &*arg_decls.entry(arity).or_insert_with(|| {
				let mut args = String::new();
				for _ in 0 .. arity {
					if !args.is_empty() {
						args.push(' ');
					}
					args.push_str("(param v128)");
				}
				args
			});
			imports.push(format!(
				"(import \"host\" \"{name}\" (func $host_{name} {args} (result v128)))\n"
			));
		}

		let mut wat_module = String::from("(module\n");
		for item in imports.into_iter().chain(globals).chain(functions) {
			wat_module.write_str(&item)?;
		}
		wat_module.write_str(")")?;

		let globals = self.globals;
		let arguments = self.arguments;
		let num_ifs_functions = module.functions.len();
		let output = WasmOutput {
			globals,
			arguments,
			num_ifs_functions,
			wat_module,
		};
		Ok(output)
	}

	fn compile_node(&mut self, expr: &Expr) -> AResult<Self::NodeOutput> {
		match expr {
			&Expr::Constant(value) => {
				let (re, im) = match value {
					Value::Real(val) => (val, 0.0),
					Value::Complex(Complex { re, im }) => (re, im),
				};
				self.current_function
					.write_fmt(format_args!("v128.const f64x2 {re} {im}\n"))?;
			},
			Expr::Variable(name) => {
				if self.globals.contains(name) {
					self.current_function
						.write_fmt(format_args!("global.get ${name}\n"))?;
				} else if self.arguments.contains(name) {
					self.current_function
						.write_fmt(format_args!("local.get ${name}\n"))?;
				} else {
					bail!("trying to read undefined variable {name:?}")
				}
			},
			Expr::Add(l, r) |
			Expr::Sub(l, r) |
			Expr::Mul(l, r) |
			Expr::Div(l, r) |
			Expr::Pow(l, r) => {
				self.compile_node(&l)?;
				self.compile_node(&r)?;
				match expr {
					Expr::Add(..) => {
						self.current_function.write_str("f64x2.add\n")?;
					},
					Expr::Sub(..) => {
						self.current_function.write_str("f64x2.sub\n")?;
					},
					Expr::Mul(..) => {
						todo!()
					},
					Expr::Div(..) => {
						todo!()
					},
					Expr::Pow(..) => {
						todo!()
					},
					_ => unreachable!(),
				}
			},
			Expr::Neg(expr) => {
				self.compile_node(expr)?;
				self.current_function.write_str("f64x2.neg\n")?;
			},
			Expr::Call { function, args } => {
				let arity = args.len();
				for arg in args {
					self.compile_node(arg)?;
				}

				// TODO: intrinsics
				if let Some(last_arity) = self.host_functions.insert(function.clone(), arity) {
					ensure!(
						arity == last_arity,
						"host function {function:?} has disallowed varying arity ({last_arity} / \
						 {arity})"
					);
				}
				self.current_function
					.write_fmt(format_args!("call $host_{function}\n"))?;
			},
		}
		Ok(())
	}
}
