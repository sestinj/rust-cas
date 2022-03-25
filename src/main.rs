// * Expressions are all tensors
// * Operations don't exist: they are all just different structs that implement the Expression trait
// * There is a macro for defining Operations
// * Instead of ever using match statements, we call the evaluate function on the Operation struct instance itself

use std::fmt;
use std::ops;

trait Expression: fmt::Debug + fmt::Display {
    fn args(&self) -> &Vec<Box<dyn Expression>>;
    fn arg(&self, idx: usize) -> &Box<dyn Expression>;
    fn reduce(&self, args: Vec<f64>) -> f64;
    fn dims(&self) -> &Vec<u128>;
    fn eval(&self) -> f64;
}

// EXAMPLE OF WHAT IS DONE BELOW BY MACROS

// struct Add {
//     args: Vec<Box<dyn Expression>>,
//     dims: Vec<u128>
// }

// impl fmt::Display for Add {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         return write!(f, "({}, {})", self.arg(0), self.arg(1));
//     }
// }

// impl fmt::Debug for Add {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         return write!(f, "({}, {})", self.arg(0), self.arg(1));
//     }
// }

// impl Expression for Add {
//     fn args(&self) -> &Vec<Box<dyn Expression>> {
//         return &self.args;
//     }
//     fn arg(&self, idx: usize) -> &Box<dyn Expression> {
//         return &self.args[idx];
//     }
//     fn dims(&self) -> &Vec<u128> {
//         return &self.dims;
//     }
//     fn reduce(&self, args: Vec<f64>) -> f64 {
//         return args[0] + args[1];
//     }
//     fn eval(&self) -> f64 {
//         let mut evaluated = vec![];
//         for arg in self.args() {
//             evaluated.push(arg.eval());
//         }
//         return self.reduce(evaluated);
//     }
// }

// MACROS

#[macro_export]
macro_rules! define_fn_struct_struct {
    ($ident:ident) => {
        struct $ident {
            args: Vec<Box<dyn Expression>>,
            dims: Vec<u128>
        }
    };
}

#[macro_export]
macro_rules! define_fn_struct_impl_fmt {
    ($ident:ident, $name:literal) => {
        impl fmt::Display for $ident {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                // TODO - Also same below for Debug
                return write!(f, "{}({})", $name, format_vec(self.args()));
            }
        }
        
        impl fmt::Debug for $ident {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                return write!(f, "{}({})", $name, format_vec(self.args()));
            }
        }
    };
}

#[macro_export]
macro_rules! define_fn_struct_impl_expr {
    ($ident:ident, $eval:expr) => {
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
            fn reduce(&self, args: Vec<f64>) -> f64 {
                return $eval(args);
            }
            fn eval(&self) -> f64 {
                let mut evaluated = vec![];
                for arg in self.args() {
                    evaluated.push(arg.eval());
                }
                return self.reduce(evaluated);
            }
        }
    };
} 

#[macro_export]
macro_rules! define_fn_struct_impl {
    ($ident:ident, $name:literal, $eval:expr) => {
        define_fn_struct_impl_fmt!($ident, $name);
        define_fn_struct_impl_expr!($ident, $eval);
    };
}

#[macro_export]
macro_rules! define_fn_struct {
    ($ident:ident, $name:literal, $eval:expr) => {
        define_fn_struct_struct!($ident);
        define_fn_struct_impl!($ident, $name, $eval);
    };
}

#[macro_export]
macro_rules! define_fn_fn {
    ($ident:ident, $name:literal, $eval:expr) => {
        fn $ident(a: Box<dyn Expression>) -> Box<$ident> {
            let dims = copy_vec(a.dims());
            return Box::new($ident{args: vec![a], dims: dims})
        }
    };
}

#[macro_export]
macro_rules! define_fn {
    ($ident:ident, $name:literal, $eval:expr) => {
        define_fn_struct!($ident, $name, $eval);
        define_fn_fn!($ident, $name, $eval);
    };
}

#[macro_export]
macro_rules! define_binary_fn {
    ($ident:ident, $name:literal, $op:tt, $op_ident:ident) => {
        define_fn_struct!($ident, $name, |args: Vec<f64>| { args[0] $op args[1] });

        fn $ident(lhs: Box<dyn Expression>, rhs: Box<dyn Expression>) -> Box<$ident> {
            // TODO - run checks on dimensions here always?? In which case you want to return Result<f64> from eval(), not f64
            let dims = copy_vec(lhs.dims());
            return Box::new($ident{args: vec![lhs, rhs], dims: dims})
        }

        impl ops::$ident<f64> for Box<dyn Expression> {
            type Output = Box<dyn Expression>;
            
            fn $op_ident(self, _rhs: f64) -> Box<dyn Expression> {
                return $ident(self, Value(_rhs));
            }
        }
        
        impl ops::$ident<Box<dyn Expression>> for f64 {
            type Output = Box<dyn Expression>;
            
            fn $op_ident(self, _rhs: Box<dyn Expression>) -> Box<dyn Expression> {
                return $ident(Value(self), _rhs);
            }
        }

        impl ops::$ident<Box<dyn Expression>> for Box<dyn Expression> {
            type Output = Box<dyn Expression>;
            
            fn $op_ident(self, _rhs: Box<dyn Expression>) -> Box<dyn Expression> {
                return $ident(self, _rhs);
            }
        }
    };
}

// Vec util functions that shouldn't exist
fn copy_vec<T>(vec: &Vec<T>) -> Vec<T> where T: Copy {
    let mut copy = vec![];
    for item in vec {
        copy.push(*item);
    }
    return copy
}

fn format_vec<T>(vec: &Vec<T>) -> String where T: fmt::Display + fmt::Debug {
    let mut string = String::new();
    if vec.len() == 0 {
        return string;
    }
    for item in vec {
        string = format!("{}{}, ", string, item);
    }
    return format!("{}", string);
}

// EVALUATE

// STANDARD FUNCTIONS
define_fn!(Sin, "Sin", |args: Vec<f64>| { args[0].sin() });

// BINARY OPS

define_binary_fn!(Add, "Add", +, add);

// SPECIAL FUNCTIONS

struct Value {
    args: Vec<Box<dyn Expression>>,
    dims: Vec<u128>,
    val: f64
}

fn Value(val: f64) -> Box<dyn Expression> {
    return Box::new(Value {args: vec![], dims: vec![1], val})
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO - Also same below for Debug
        return write!(f, "{}", self.val);
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return write!(f, "{}", self.val);
    }
}

impl Expression for Value {
    fn args(&self) -> &Vec<Box<dyn Expression>> {
        return &self.args;
    }
    fn arg(&self, idx: usize) -> &Box<dyn Expression> {
        return &self.args[idx];
    }
    fn dims(&self) -> &Vec<u128> {
        return &self.dims;
    }
    fn reduce(&self, _args: Vec<f64>) -> f64 {
        return self.val;
    }
    fn eval(&self) -> f64 {
        let mut evaluated = vec![];
        for arg in self.args() {
            evaluated.push(arg.eval());
        }
        return self.reduce(evaluated);
    }
}


struct Variable {
    args: Vec<Box<dyn Expression>>,
    dims: Vec<u128>,
    id: u128,
    symbol: String
}

struct VariableIdCounter {
    curr: u128
}

impl VariableIdCounter {
    fn get_next(&mut self) -> u128 {
        self.curr += 1;
        return self.curr
    }
}

const VARIABLE_ID_COUNTER: VariableIdCounter = VariableIdCounter {curr: 0};

fn Variable(symbol: String) -> Box<dyn Expression> {
    return Box::new(Variable {args: vec![], dims: vec![1], symbol: symbol, id: VARIABLE_ID_COUNTER.get_next()})
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO - Also same below for Debug
        return write!(f, "{}", self.symbol);
    }
}

impl fmt::Debug for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return write!(f, "{}", self.symbol);
    }
}

impl Expression for Variable {
    fn args(&self) -> &Vec<Box<dyn Expression>> {
        return &self.args;
    }
    fn arg(&self, idx: usize) -> &Box<dyn Expression> {
        return &self.args[idx];
    }
    fn dims(&self) -> &Vec<u128> {
        return &self.dims;
    }
    fn reduce(&self, _args: Vec<f64>) -> f64 {
        return 0.0; // TODO! This is a special situation. You'll actually throw an error (Result::Err) here
        // because reduce shouldn't ever be called on a Variable.
    }
    fn eval(&self) -> f64 {
        let mut evaluated = vec![];
        for arg in self.args() {
            evaluated.push(arg.eval());
        }
        return self.reduce(evaluated);
    }
}

fn main() {
    let theta = Value(std::f64::consts::FRAC_PI_3);
    let s = Sin(theta);
    let a = Value(5.0);
    let b = Value(6.0);
    let t = a + b;
    // let t = Add(a, b);
    println!("{} = {}", s, s.eval());
    println!("{} = {}", t, t.eval());
    // let ss = Sin(theta); // ??? Tough because Box doesn't implement copy
}


// MACRO TREE
///
/// Granular customization by specifying exactly which portion you want to specify, and it calculates which other macros to run
/// For example, if I only want to customize define_fn_struct_impl_expr_evaluate, then I say so and the tree does all of define_fn_fn, define_fn_struct_struct, define_fn_struct_impl_expr_{siblings}, and then finally this one in its correct order.
/// Or just better to define structs with defaults??
struct MacroTree {
    subs: Vec<MacroTree>,
    // run: How do I represent the type of a macro???
}

// TODO
// * Swap out ndarray for f64
// * macro for general arity
// * Copyable Expression pointers? Can you fix this by just making expressions Copyable?


// * dims is unecessary?