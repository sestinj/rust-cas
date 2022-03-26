// * Expressions are all tensors
// * Operations don't exist: they are all just different structs that implement the Expression trait
// * There is a macro for defining Operations
// * Instead of ever using match statements, we call the evaluate function on the Operation struct instance itself

use std::fmt;
use std::ops;
use std::collections::HashMap;
use std::any::Any;

static mut EXPRESSION_ID_COUNTER: u128 = 0;

fn get_next_expr_id() -> u128 {
    unsafe {
        EXPRESSION_ID_COUNTER += 1;
        return EXPRESSION_ID_COUNTER
    }
    
}

trait Expression: fmt::Debug + fmt::Display {
    fn args(&self) -> &Vec<Box<dyn Expression>>;
    fn arg(&self, idx: usize) -> &Box<dyn Expression>;
    fn reduce(&self, args: Vec<f64>) -> f64;
    fn dims(&self) -> &Vec<u128>;
    fn eval(&self, vars: &HashMap<u128, f64>) -> f64;
    fn fits(&self, pattern: &Box<dyn Expression>) -> bool;
    fn id(&self) -> u128;
    fn as_any(&self) -> &dyn Any;
}

struct Identity {
    lhs: Box<dyn Expression>,
    rhs: Box<dyn Expression>
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
            dims: Vec<u128>,
            id: u128
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
            fn eval(&self, vars: &HashMap<u128, f64>) -> f64 {
                let mut evaluated = vec![];
                for arg in self.args() {
                    evaluated.push(arg.eval(vars));
                }
                return self.reduce(evaluated);
            }
            fn id(&self) -> u128 {
                return self.id
            }
            fn as_any(&self) -> &dyn Any {
                self
            }
            // fn sub(&self, identities: Vec<Identity>) -> Box<dyn Expression> {
            //     for identity in identities {
            //         if
            //     }
            // }

            /// Check whether the expression fits a pattern
            /// Two expressions are considered to fit iff
            /// 1. They are the same function
            /// 2. Every argument that is not a placeholder in the pattern fits that argument in the pattern
            /// 3. Whatever expression is found in the place of a placeholder must be a fit with every other argument that has the same placeholder id
            fn fits(&self, pattern: &Box<dyn Expression>) -> bool {
                if self.id != pattern.id() {
                    return false;
                }
                // HashMap keeps track of the expression associated with a placeholder. If we ever find a mismatch, immediately return false.
                let placeholders: HashMap<u128, &Box<dyn Expression>> = HashMap::new();
                for i in 0..pattern.args().len() {
                    let patt_arg = pattern.arg(i);
                    let expr_arg = self.arg(i);
                    if let Some(casted_placeholder) = patt_arg.as_any().downcast_ref::<Placeholder>() {
                        // If it's a placeholder, check the HashMap
                        if let Some(placeholder_pattern) = placeholders.get(&casted_placeholder.placeholder_id) {
                            // If we've already seen this placeholder but the expressions don't match, return false
                            if !placeholder_pattern.fits(expr_arg) {
                                return false;
                            }
                        } else {
                            // New placeholder, add to the HashMap
                            placeholders.insert(casted_placeholder.placeholder_id, expr_arg);
                        }
                    } else {
                        // If it's not a placeholder, must fit exactly
                        if !expr_arg.fits(patt_arg) {
                            return false;
                        }
                    }
                }
                return true;
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
            return Box::new($ident{args: vec![a], dims: dims, id: get_next_expr_id()})
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
            return Box::new($ident{args: vec![lhs, rhs], dims: dims, id: get_next_expr_id()})
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

        impl ops::$ident<u128> for Box<dyn Expression> {
            type Output = Box<dyn Expression>;
            
            fn $op_ident(self, _rhs: u128) -> Box<dyn Expression> {
                return $ident(self, Value(_rhs as f64));
            }
        }
        
        impl ops::$ident<Box<dyn Expression>> for u128 {
            type Output = Box<dyn Expression>;
            
            fn $op_ident(self, _rhs: Box<dyn Expression>) -> Box<dyn Expression> {
                return $ident(Value(self as f64), _rhs);
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

#[macro_export]
macro_rules! define_unary_f64_fn {
    ($ident:ident, $name:literal, $method_ident:ident) => {
        define_fn!($ident, $name, | args: Vec<f64> | { args[0].$method_ident() });
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
    return format!("{}", &string[0..string.len() - 2]);
}

// EVALUATE

// STANDARD FUNCTIONS
define_fn!(Sin, "Sin", |args: Vec<f64>| { args[0].sin() });
define_unary_f64_fn!(Cos, "Cos", cos);

// BINARY OPS

define_binary_fn!(Add, "Add", +, add);
define_binary_fn!(Sub, "Sub", -, sub);
define_binary_fn!(Mul, "Mul", *, mul);
define_binary_fn!(Div, "Div", /, div);

// SPECIAL FUNCTIONS

struct Value {
    args: Vec<Box<dyn Expression>>,
    dims: Vec<u128>,
    val: f64,
    id: u128
}

const VALUE_ID: u128 = get_next_expr_id();

fn Value(val: f64) -> Box<dyn Expression> {
    return Box::new(Value {args: vec![], dims: vec![1], val, id: VALUE_ID})
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
    fn eval(&self, vars: &HashMap<u128, f64>) -> f64 {
        let mut evaluated = vec![];
        for arg in self.args() {
            evaluated.push(arg.eval(vars));
        }
        return self.reduce(evaluated);
    }
    fn fits(&self, pattern: Box<dyn Expression>) -> bool {
        if self.
        return true;
    }
}


struct Variable {
    args: Vec<Box<dyn Expression>>,
    dims: Vec<u128>,
    id: u128,
    var_id: u128,
    symbol: String
}

static mut VARIABLE_ID_COUNTER: u128 = 0;

fn get_next_id() -> u128 {
    unsafe {
        VARIABLE_ID_COUNTER += 1;
        return VARIABLE_ID_COUNTER
    }
    
}

const VARIABLE_ID: u128 = get_next_expr_id();

fn Variable(symbol: &str) -> Box<dyn Expression> {
    return Box::new(Variable {args: vec![], dims: vec![1], symbol: symbol.to_string(), var_id: get_next_id(), id: VARIABLE_ID})
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
    fn eval(&self, vars: &HashMap<u128, f64>) -> f64 {
        return vars[&self.id];
        // Two things you can do: 1) what you're doing here, treat eval specially for variables or 2) that described below
        // Move the below into a method on Expression called try_sub
        // This is where you need the substitution step. Giving a variable a value is the same as defining an identity
        // You must search for all matches and replace. In this function, since it aims to return f64 only, you can throw
        // if not everything is simplified after checking all patterns.
    }

}

struct Placeholder {
    args: Vec<Box<dyn Expression>>,
    dims: Vec<u128>,
    id: u128,
    placeholder_id: u128,
    symbol: String
}

static mut PLACEHOLDER_ID_COUNTER: u128 = 0;

const PLACEHOLDER_ID: u128 = get_next_expr_id();

fn Placeholder(symbol: &str) -> Box<dyn Expression> {
    PLACEHOLDER_ID_COUNTER += 1;
    return Box::new(Placeholder {args: vec![], dims: vec![1], symbol: symbol.to_string(), placeholder_id: PLACEHOLDER_ID_COUNTER, id: PLACEHOLDER_ID})
}

impl fmt::Display for Placeholder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return write!(f, "[{}]", self.placeholder_id);
    }
}

impl fmt::Debug for Placeholder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return write!(f, "[{}]", self.placeholder_id);
    }
}

impl Expression for Placeholder {
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
        panic!("Can't reduce placeholder");
    }
    fn eval(&self, vars: &HashMap<u128, f64>) -> f64 {
        panic!("Can't eval placeholder");
    }
    fn id(&self) -> u128 {
        return self.id
    }

    fn fits(&self, pattern: Box<dyn Expression>) -> bool {
        
    }
}

fn main() {
    let theta = Value(std::f64::consts::FRAC_PI_3);
    let s = Sin(theta);
    let a = Value(5.0);
    let b = Value(6.0);
    let t = a + b;
    // let t = Add(a, b);
    println!("{} = {}", s, s.eval(&HashMap::new()));
    println!("{} = {}", t, t.eval(&HashMap::new()));
    // let ss = Sin(theta); // ??? Tough because Box doesn't implement copy

    let c = Variable("c");
    let d = Variable("d");
    let expr3 = (2 * c + d - 10) / 2;
    println!("{} = {}", expr3, expr3.eval(&HashMap::from([(1, 4.0), (2, 5.0)])));

    let co = Cos(Value(0.0));
    println!("{} = {}", co, co.eval(&HashMap::new()));
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