use std::{cell::RefCell, sync::LazyLock};

use anyhow::{anyhow, bail, ensure};
use num::{Complex, complex::Complex64};
use wasmtime::{Config, Engine, Func, Instance, IntoFunc, Linker, Module, Store, V128, Val};

use crate::{AResult, compiler::WasmOutput, lexer::Value};

pub struct WasmRuntime {
	store: Store<()>,
	instance: Instance,
	functions: Vec<Func>,
}

impl WasmRuntime {
	pub fn builder() -> WasmRuntimeBuilder {
		WasmRuntimeBuilder::new().expect("could not construct WasmRuntimeBuilder")
	}

	pub fn get_variable(&mut self, name: &str) -> AResult<Complex64> {
		let global = self
			.instance
			.get_global(&mut self.store, name)
			.ok_or_else(|| anyhow!("no variable named {name:?}"))?;
		let val = global.get(&mut self.store);
		val.v128()
			.map(unpack_v128)
			.ok_or_else(|| anyhow!("variable {name:?} is not a V128(??!)"))
	}

	pub fn set_variable(&mut self, name: &str, value: Value) -> AResult<()> {
		let value = pack_v128(value);
		let global = self
			.instance
			.get_global(&mut self.store, name)
			.ok_or_else(|| anyhow!("no variable named {name:?}"))?;
		global.set(&mut self.store, value.into())?;
		Ok(())
	}

	pub fn call(&mut self, function: usize, arguments: &[Value]) -> AResult<Complex64> {
		ensure!(
			function < self.functions.len(),
			"trying to call IFS function {function} but only {} are defined",
			self.functions.len()
		);

		// FIXME:
		thread_local! {
			static ARGUMENTS: RefCell<Vec<Val>> = RefCell::new(vec![]);
		}
		ARGUMENTS.with_borrow_mut(|argument_vals| {
			argument_vals.clear();
			argument_vals.extend(arguments.into_iter().map(|&v| Val::from(pack_v128(v))));
			let mut result = Val::V128(V128::from(0));
			self.functions[function].call(
				&mut self.store,
				argument_vals,
				std::slice::from_mut(&mut result),
			)?;
			let Val::V128(result) = result else {
				bail!("IFS function {function} returned something other than V128")
			};
			Ok(unpack_v128(result))
		})
	}
}

static ENGINE: LazyLock<Engine> = LazyLock::new(|| {
	let mut config = Config::new();
	config.wasm_shared_everything_threads(true);
	Engine::new(&config).expect("could not construct wasmtime::Engine")
});

pub struct WasmRuntimeBuilder {
	store: Store<()>,
	linker: Linker<()>,
}

impl WasmRuntimeBuilder {
	pub fn new() -> AResult<Self> {
		let store = Store::new(&ENGINE, ());
		let linker = Linker::new(&ENGINE);
		Ok(Self { store, linker })
	}

	pub fn build(self, module: &WasmOutput) -> AResult<WasmRuntime> {
		let Self { mut store, linker } = self;
		let &WasmOutput {
			num_ifs_functions,
			ref wasm_module,
			..
		} = module;
		let module = Module::new(&ENGINE, wasm_module)?;
		let instance = linker.instantiate(&mut store, &module)?;
		let functions = (0 .. num_ifs_functions)
			.map(|index| {
				let name = format!("f{index}");
				instance.get_func(&mut store, &name).ok_or_else(|| {
					anyhow!(
						"ifs function {index} is not defined (expected {num_ifs_functions} total)"
					)
				})
			})
			.collect::<AResult<Vec<_>>>()?;
		Ok(WasmRuntime {
			store,
			instance,
			functions,
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
