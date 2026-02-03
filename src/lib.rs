pub mod compiler;
pub mod lexer;
pub mod parser;

use crate::parser::Module;

pub type AResult<T> = anyhow::Result<T>;

pub fn parse_module(code: &str) -> AResult<Module> {
	let tokens = self::lexer::lex(code)?;
	let mut module = self::parser::parse(&tokens)?;
	module.propagate_constants();
	module.fold_constants();
	Ok(module)
}

pub fn compile<Compiler: self::compiler::Compiler + Default>(
	module: &Module,
) -> AResult<Compiler::Output> {
	let mut compiler: Compiler = Default::default();
	compiler.compile_module(module)
}
