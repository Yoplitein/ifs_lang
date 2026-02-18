use std::collections::HashMap;

use anyhow::Ok;
use num::Complex;

use super::Compiler;
use crate::{
	AResult,
	lexer::Value,
	parser::{Expr, Module},
};

pub struct WasmCompiler {
	global_indices: HashMap<String, u32>,
	current_function: wasm_encoder::Function,
}

impl Default for WasmCompiler {
	fn default() -> Self {
		Self {
			global_indices: HashMap::new(),
			current_function: wasm_encoder::Function::new(vec![]),
		}
	}
}

pub struct WasmOutput {
	pub global_indices: HashMap<String, u32>,
	pub arguments: Vec<String>,
	pub wasm_module: Vec<u8>,
}

#[derive(Default)]
struct WasmModule {
	types: wasm_encoder::TypeSection,
	functions: wasm_encoder::FunctionSection,
	globals: wasm_encoder::GlobalSection,
	exports: wasm_encoder::ExportSection,
	code: wasm_encoder::CodeSection,
}

impl WasmModule {
	fn finish(self) -> Vec<u8> {
		let mut module = wasm_encoder::Module::new();
		module.section(&self.types);
		module.section(&self.functions);
		module.section(&self.globals); // FIXME: correct order?
		module.section(&self.exports);
		module.section(&self.code);
		module.finish()
	}
}

impl Compiler for WasmCompiler {
	type NodeOutput = ();
	type Output = WasmOutput;

	fn compile_module(mut self, module: &Module) -> AResult<Self::Output> {
		let mut wasm_module = WasmModule::default();

		for name in &module.globals {
			let global_index = wasm_module.globals.len();
			wasm_module.globals.global(
				wasm_encoder::GlobalType {
					val_type: wasm_encoder::ValType::V128,
					mutable: true,
					shared: true,
				},
				&wasm_encoder::ConstExpr::v128_const(1),
			);
			let None = self.global_indices.insert(name.clone(), global_index) else {
				anyhow::bail!("global {name:?} defined more than once")
			};
		}

		let ifs_func_type = wasm_module.types.len();
		wasm_module.types.ty().function(
			(0 .. module.arguments.len()).map(|_| wasm_encoder::ValType::V128),
			[wasm_encoder::ValType::V128],
		);

		for (index, func) in module.functions.iter().enumerate() {
			self.compile_node(&func.body)?;
			let mut compiled_func = std::mem::replace(
				&mut self.current_function,
				wasm_encoder::Function::new(vec![]),
			);
			compiled_func.instructions().end();
			let code_index = wasm_module.code.len();
			wasm_module.code.function(&compiled_func);
			let func_index = wasm_module.functions.len();
			assert_eq!(code_index, func_index);
			wasm_module.functions.function(ifs_func_type);
			wasm_module.exports.export(
				&format!("f{index}"),
				wasm_encoder::ExportKind::Func,
				func_index,
			);
		}

		let global_indices = self.global_indices;
		let arguments = module.arguments.clone();
		let wasm_module = wasm_module.finish();
		let output = WasmOutput {
			global_indices,
			arguments,
			wasm_module,
		};
		Ok(output)
	}

	fn compile_node(&mut self, expr: &Expr) -> AResult<Self::NodeOutput> {
		match expr {
			Expr::Constant(value) => {
				let literal = match value {
					Value::Real(val) => val.to_bits() as _,
					Value::Complex(val) => {
						let Complex { re, im } = val;
						let (re, im) = (re.to_bits() as u128, im.to_bits() as u128);
						(im << 64 | re) as _
					},
				};
				self.current_function.instructions().v128_const(literal);
			},
			Expr::Variable(name) => {
				let Some(&index) = self.global_indices.get(name) else {
					anyhow::bail!("reading undefined global {name:?}")
				};
				self.current_function.instructions().global_get(index);
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
						self.current_function.instructions().f64x2_add();
					},
					Expr::Sub(..) => {
						self.current_function.instructions().f64x2_sub();
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
				self.current_function.instructions().f64x2_neg();
			},
			Expr::Call { function, args } => {
				todo!()
			},
		}
		Ok(())
	}
}
