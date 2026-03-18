use std::{
	collections::HashMap,
	sync::{Arc, LazyLock},
};

use anyhow::{anyhow, bail, ensure};
use num::{Complex, complex::Complex64};
pub use wasmtime;
use wasmtime::{Config, Engine, Func, Instance, IntoFunc, Linker, Module, Store, V128, Val};

use crate::{AResult, compiler::WasmOutput, lexer::Value};

pub struct ThreadInstance {
	store: Store<()>,
	instance: Instance,
	functions: Vec<Func>,
}

impl ThreadInstance {
	pub fn update_variables(&mut self, runtime: &WasmRuntime) -> AResult<()> {
		for (name, &value) in &runtime.variables {
			let global = self
				.instance
				.get_global(&mut self.store, name)
				.ok_or_else(|| anyhow!("trying to set undefined global {name:?}"))?;
			global.set(&mut self.store, pack_v128(value).into())?;
		}
		Ok(())
	}

	pub fn call(&mut self, runtime: &WasmRuntime, function: usize) -> AResult<Complex64> {
		ensure!(
			function < self.functions.len(),
			"trying to call IFS function {function} but only {} are defined",
			self.functions.len()
		);
		let mut result = Val::V128(V128::from(0));
		self.functions[function].call(
			&mut self.store,
			&runtime.arguments,
			std::slice::from_mut(&mut result),
		)?;
		let Val::V128(result) = result else {
			bail!("IFS function {function} returned something other than V128")
		};
		Ok(unpack_v128(result))
	}
}

pub struct WasmRuntime {
	linker: Arc<Linker<()>>,
	module: Arc<Module>,
	num_ifs_functions: usize,
	num_arguments: usize,
	arguments: Vec<Val>,
	variables: HashMap<String, Value>,
}

impl WasmRuntime {
	pub fn builder() -> WasmRuntimeBuilder {
		WasmRuntimeBuilder::new()
	}

	pub fn set_module(&mut self, module: &WasmOutput) -> AResult<()> {
		let module = Module::new(&ENGINE, &module.wat_module)?;
		self.module = Arc::new(module);
		Ok(())
	}

	pub fn instantiate(&self, num_threads: usize) -> AResult<Vec<ThreadInstance>> {
		(0 .. num_threads)
			.map(|_| {
				let mut store = Store::new(&ENGINE, ());
				let instance = self.linker.instantiate(&mut store, &self.module)?;
				let functions = (0 .. self.num_ifs_functions)
					.map(|index| {
						let name = format!("f{index}");
						instance.get_func(&mut store, &name).ok_or_else(|| {
							anyhow!(
								"ifs function {index} is not defined (expected {} total)",
								self.num_ifs_functions,
							)
						})
					})
					.collect::<AResult<Vec<_>>>()?;
				Ok(ThreadInstance {
					store,
					instance,
					functions,
				})
			})
			.collect()
	}

	pub fn get_arguments(&self) -> impl Iterator<Item = Complex64> {
		self.arguments.iter().map(|&v| {
			let Val::V128(v) = v else {
				unreachable!("arguments are enforced to be of type Val::V128 by set_arguments")
			};
			unpack_v128(v)
		})
	}

	pub fn set_arguments(&mut self, args: &[Value]) -> AResult<()> {
		ensure!(
			args.len() == self.num_arguments,
			"trying to set arguments with {} values, need {}",
			args.len(),
			self.num_arguments
		);
		self.arguments.clear();
		let args = args.into_iter().map(|&v| Val::from(pack_v128(v)));
		self.arguments.extend(args);
		Ok(())
	}

	pub fn get_variable(&self, name: &str) -> AResult<Value> {
		self.variables
			.get(name)
			.copied()
			.ok_or_else(|| anyhow!("no variable named {name:?}"))
	}

	pub fn set_variable(&mut self, name: &str, value: Value) {
		self.variables.insert(name.into(), value);
	}
}

static ENGINE: LazyLock<Engine> = LazyLock::new(|| {
	let mut config = Config::new();
	config.wasm_shared_everything_threads(true);
	Engine::new(&config).expect("could not construct wasmtime::Engine")
});

pub struct WasmRuntimeBuilder {
	linker: Linker<()>,
}

impl WasmRuntimeBuilder {
	pub fn new() -> Self {
		let linker = Linker::new(&ENGINE);
		Self { linker }
	}

	pub fn build(self, module: &WasmOutput) -> AResult<WasmRuntime> {
		let Self { linker } = self;
		let linker = Arc::new(linker);
		let num_arguments = module.arguments.len();
		let &WasmOutput {
			num_ifs_functions,
			wat_module: ref wasm_module,
			..
		} = module;
		let module = Module::new(&ENGINE, wasm_module)?;
		let module = Arc::new(module);
		Ok(WasmRuntime {
			linker,
			module,
			num_arguments,
			num_ifs_functions,
			arguments: Vec::with_capacity(num_arguments),
			variables: HashMap::new(),
		})
	}

	pub fn define_host_function<Params, Results>(
		mut self,
		name: &str,
		func: impl IntoFunc<(), Params, Results>,
	) -> AResult<Self> {
		self.linker.func_wrap("host", name, func)?;
		Ok(self)
	}
}

pub fn pack_v128(value: Value) -> V128 {
	let (x, y) = match value {
		Value::Real(v) => (v.to_bits(), 0),
		Value::Complex(Complex { re, im }) => (re.to_bits(), im.to_bits()),
	};
	let (x, y) = (x as u128, y as u128);
	V128::from(x | y << 64)
}

pub fn unpack_v128(v: V128) -> Complex64 {
	let v = v.as_u128();
	let (x, y) = (v as u64, (v >> 64) as u64);
	Complex64 {
		re: f64::from_bits(x),
		im: f64::from_bits(y),
	}
}
