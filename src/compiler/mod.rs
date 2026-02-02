use num::Complex;
use strum::IntoDiscriminant;

use crate::{
	lexer::Value,
	parser::{Expr, VariableMap},
};

impl From<f64> for Value {
	fn from(value: f64) -> Self {
		Self::Real(value)
	}
}

impl From<Complex<f64>> for Value {
	fn from(value: Complex<f64>) -> Self {
		Self::Complex(value)
	}
}

macro_rules! impl_value_op {
	($trait:tt, $method:ident, $op:tt) => {
		impl std::ops::$trait for Value {
			type Output = Self;

			fn $method(self, rhs: Self) -> Self::Output {
				match (self, rhs) {
					(Self::Real(l), Self::Real(r)) => {
						Self::Real(l $op r)
					},
					(Self::Real(l), Self::Complex(r)) => {
						Self::Complex(l $op r)
					},
					(Self::Complex(l), Self::Real(r)) => {
						Self::Complex(l $op r)
					},
					(Self::Complex(l), Self::Complex(r)) => {
						Self::Complex(l $op r)
					},
					_ => todo!(),
				}
			}
		}
	};
}

impl_value_op!(Add, add, +);
impl_value_op!(Sub, sub, -);
impl_value_op!(Mul, mul, *);
impl_value_op!(Div, div, /);

impl Value {
	pub fn pow(&self, rhs: Self) -> Self {
		match (*self, rhs) {
			(Value::Real(l), Value::Real(r)) => l.powf(r).into(),
			(Value::Real(l), Value::Complex(r)) => Complex::new(l, 0.0).powc(r).into(),
			(Value::Complex(l), Value::Real(r)) => l.powf(r).into(),
			(Value::Complex(l), Value::Complex(r)) => l.powc(r).into(),
		}
	}
}

impl Expr {
	pub fn is_constant(&self) -> bool {
		matches!(self, Self::Constant(_))
	}

	pub fn into_value(&self) -> Value {
		let &Self::Constant(value) = self else {
			panic!("trying to `.into_value` an Expr::{:?}", self.discriminant())
		};
		value
	}

	/// Substitutes variables defined as constants with their value.
	pub fn propagate_constants(&mut self, constants: &VariableMap) {
		match self {
			Self::Constant(_) => {},
			Self::Variable(variable) => {
				if let Some(&value) = constants.get(variable) {
					*self = Self::Constant(value);
				}
			},
			Self::Add(l, r) |
			Self::Sub(l, r) |
			Self::Mul(l, r) |
			Self::Div(l, r) |
			Self::Pow(l, r) => {
				l.propagate_constants(constants);
				r.propagate_constants(constants);
			},
			Self::Neg(expr) => expr.propagate_constants(constants),
			Self::Call { args, .. } => {
				for expr in args {
					expr.propagate_constants(constants);
				}
			},
		}
	}

	/// Substitutes subexpressions containing only constants as terms with the
	/// resulting value.
	pub fn fold_constants(&mut self) {
		match self {
			Expr::Add(l, r) => {
				l.fold_constants();
				r.fold_constants();
				if l.is_constant() && r.is_constant() {
					let (l, r) = (l.into_value(), r.into_value());
					*self = Self::Constant(l + r);
				}
			},
			Expr::Sub(l, r) => {
				l.fold_constants();
				r.fold_constants();
				if l.is_constant() && r.is_constant() {
					let (l, r) = (l.into_value(), r.into_value());
					*self = Self::Constant(l - r);
				}
			},
			Expr::Mul(l, r) => {
				l.fold_constants();
				r.fold_constants();
				if l.is_constant() && r.is_constant() {
					let (l, r) = (l.into_value(), r.into_value());
					*self = Self::Constant(l * r);
				}
			},
			Expr::Div(l, r) => {
				l.fold_constants();
				r.fold_constants();
				if l.is_constant() && r.is_constant() {
					let (l, r) = (l.into_value(), r.into_value());
					*self = Self::Constant(l / r);
				}
			},
			Expr::Pow(l, r) => {
				l.fold_constants();
				r.fold_constants();
				if l.is_constant() && r.is_constant() {
					let (l, r) = (l.into_value(), r.into_value());
					*self = Self::Constant(l.pow(r));
				}
			},
			Expr::Neg(expr) if expr.is_constant() => todo!(),
			Expr::Call { args, .. } => {
				for arg in args {
					arg.fold_constants();
				}
				// MAYBE: eval trig functions etc?
				// for now just treat as a leaf
			},
			_ => {
				// anything else (leaf nodes) cannot be folded
			},
		}
	}
}

impl crate::parser::Module {
	pub fn propagate_constants(&mut self) {
		for function in &mut self.functions {
			function.propagate_constants();
		}
	}

	pub fn fold_constants(&mut self) {
		for function in &mut self.functions {
			function.fold_constants();
		}
	}
}

impl crate::parser::Function {
	pub fn propagate_constants(&mut self) {
		self.body.propagate_constants(&self.constants);
	}

	pub fn fold_constants(&mut self) {
		self.body.fold_constants();
	}
}

#[test]
fn skree() {
	// let q = Expr::Neg(Expr::Constant(Value::Real(1.0)).into());
	// dbg!(q.is_constant(), q.into_value());

	/* macro_rules! test {
		($l:expr, $r:expr) => {
			dbg!($l + $r);
			dbg!($l - $r);
			dbg!($l * $r);
			dbg!($l / $r);
			dbg!($l.pow($r));
		};
	}
	test!(Value::Real(2.0), Value::Real(4.0));
	test!(Value::Real(2.0), Value::Complex(Complex::new(4.0, 6.0)));
	test!(Value::Complex(Complex::new(2.0, 4.0)), Value::Real(6.0));
	test!(Value::Complex(Complex::new(2.0, 4.0)), Value::Complex(Complex::new(6.0, 8.0))); */

	let inp = r#"
		const a = 3;
		const b = 5;
		func sqrt(a ^ 2 + b ^ 2);
	"#;
	let tokens = crate::lexer::lex(inp).unwrap();
	let mut module = crate::parser::parse(&tokens).unwrap();
	dbg!(&module);
	module.propagate_constants();
	dbg!(&module);
	module.fold_constants();
	dbg!(&module);

	panic!("skree");
}
