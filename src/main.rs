// * Expressions are all tensors
// * Operations don't exist: they are all just different structs that implement the Expression trait
// * There is a macro for defining Operations
// * Instead of ever using match statements, we call the evaluate function on the Operation struct instance itself

use std::fmt;

trait Expression: fmt::Debug + fmt::Display {
    fn args(&self) -> &Vec<Box<dyn Expression>>;
    fn arg(&self, idx: usize) -> &Box<dyn Expression>;
    fn evaluate(&self, args: Vec<f64>) -> f64;
    fn dims(&self) -> &Vec<u128>;
}

// EXAMPLE OF WHAT IS DONE BELOW BY MACROS

struct Add {
    args: Vec<Box<dyn Expression>>,
    dims: Vec<u128>
}

impl fmt::Display for Add {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return write!(f, "({}, {})", self.arg(0), self.arg(1));
    }
}

impl fmt::Debug for Add {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return write!(f, "({}, {})", self.arg(0), self.arg(1));
    }
}

impl Expression for Add {
    fn args(&self) -> &Vec<Box<dyn Expression>> {
        return &self.args;
    }
    fn arg(&self, idx: usize) -> &Box<dyn Expression> {
        return &self.args[idx];
    }
    fn dims(&self) -> &Vec<u128> {
        return &self.dims;
    }
    fn evaluate(&self, args: Vec<f64>) -> f64 {
        return args[0] + args[1];
    }
}

// MACROS

#[macro_export]
macro_rules! define_fn_struct {
    ($ident:ident, $n:literal, $eval:expr) => {
        struct $ident {
            args: Vec<Box<dyn Expression>>,
            dims: Vec<u128>
        }
        
        impl fmt::Display for $ident {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                // TODO - Also same below for Debug
                return write!(f, "({}, {})", self.arg(0), self.arg(1));
            }
        }
        
        impl fmt::Debug for $ident {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                return write!(f, "({}, {})", self.arg(0), self.arg(1));
            }
        }
        
        impl Expression for $ident {
            fn args(&self) -> &Vec<Box<dyn Expression>> {
                return &self.args;
            }
            fn arg(&self, idx: usize) -> &Box<dyn Expression> {
                return &self.args[idx];
            }
            fn dims(&self) -> &Vec<u128> {
                return &self.dims;
            }
            fn evaluate(&self, args: Vec<f64>) -> f64 {
                return $eval(args);
            }
        }

        
    };
}

#[macro_export]
macro_rules! define_fn_fn {
    ($ident:ident, $n:literal, $eval:expr) => {
        fn $ident(a: Box<dyn Expression>) -> $ident {
            let mut dims = vec![];
            for dim in a.dims() {
                dims.push(*dim);
            }
            return $ident{args: vec![a], dims: dims}
        }
    };
}

#[macro_export]
macro_rules! define_fn {
    ($ident:ident, $n:literal, $eval:expr) => {
        define_fn_struct!($ident, $n, $eval);
        define_fn_fn!($ident, $n, $eval);
    };
}

#[macro_export]
macro_rules! define_binary_fn {
    () => {
        
    };
}

// EVALUATE

fn evaluate(expr: &Box<dyn Expression>) -> f64 {
    let mut evaluated = vec![];
    for arg in expr.args() {
        evaluated.push(evaluate(arg));
    }
    return expr.evaluate(evaluated);
}

// STANDARD FUNCTIONS
define_fn!(Sin, 1, |args: Vec<f64>| { args[0].sin() });

// BINARY OPS



// SPECIAL FUNCTIONS
define_fn_struct!(Value, 1, |args: Vec<f64>| { args[0] });
fn Value(val: f64) -> Box<Value> {
    return Box::new(Value {args: vec![], dims: vec![1]})
}

define_fn!(Variable, 0, |_args: Vec<f64>| { 0.0 });

fn main() {
    let theta = Value(std::f64::consts::FRAC_PI_3);
    let s = Sin(theta);
    println!("5: {}", s);
}