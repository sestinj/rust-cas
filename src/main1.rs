// * Expressions are all tensors
// * Operations don't exist: they are all just different structs that implement the Expression trait
// * There is a macro for defining Operations
// * Instead of ever using match statements, we call the evaluate function on the Operation struct instance itself

use std::{
    fmt::{self, Debug},
    ops,
    collections::{
        HashMap,
        hash_map::DefaultHasher
    },
    any::Any,
    hash::{
        Hash,
        Hasher
    },
    io::{
        stdin,
        stdout
    }
};

/// structs that implement Expression are the nodes of the AST, an operator/function with some
/// number of arguments
trait Expression: fmt::Debug + fmt::Display {
    // NOTE: Don't add methods that don't contain specific information about the function itself. For example the fits method was moved out to the shared_fits function because it is the same for all Expressions. No need to be able to call expr.fit(patt) when you can call fit(expr, patt). The other is barely even sugar.
    /// Get the arguments of the operator
    fn args(&self) -> &Vec<Box<dyn Expression>>;
    /// Get the argument at the given index
    fn arg(&self, idx: usize) -> &Box<dyn Expression>;
    /// 
    fn compute(&self, args: Vec<f64>) -> f64;
    /// Get the dimensions of the tensor represented by this expression
    fn dims(&self) -> &Vec<u128>;
    /// eval expects to COMPLETELY evaluate the expression. If it can't be fully reduced to a number, it fails.
    fn eval(&self, vars: &HashMap<u64, f64>) -> f64;
    /// reduce uses only the compute identity to evaluate as far as possible, returning an expression that may be simplified.
    /// It's like eval but instead of failing, it just leaves the unevaluated piece in the expression
    // fn reduce(&self) -> &Box<dyn Expression>; But it is being moved outside for reasons described in the note above
    // fn fits(&self, pattern: &Box<impl Expression>) -> bool;
    fn id(&self) -> u64;
    fn as_any(&self) -> &dyn Any;
    // Optimize: turn into an optimized function for super speedy calculation when given all variables. Should be highly parallelizable.
}

struct Identity {
    lhs: Box<dyn Expression>,
    rhs: Box<dyn Expression>
}

enum SpecialExprType {
    Variable,
    Value,
    Placeholder
}

fn special_expression_id(expr: SpecialExprType) -> u64 {
    match expr {
        SpecialExprType::Variable => calculate_hash(&"Variable"),
        SpecialExprType::Value => calculate_hash(&"Value"),
        SpecialExprType::Placeholder => calculate_hash(&"Placeholder")
    }
}
// Instead of making an entirely new struct for identities, what if it was just a new operator? := or ==
// I think == makes sense because you assume all equations you have are true?
// And you can attach whatever custom metadata needed. I think tags for left-to-right and right-to-left
// Tags might note whether this is a simplification in one direction, or just which modes of substitution it should be used in.

// SHARED METHOD DEFINITIONS

// TODO - REDUCE!!!! Needed for fits
    // / reduce uses only the compute identity to evaluate as far as possible, returning an expression that may be simplified
fn reduce(expr: &Box<dyn Expression>) -> &Box<dyn Expression> {
    // expr.compute(vec![]);
    return expr;//TODO
}

/// Check whether the expression fits a pattern
/// Two expressions are considered to fit iff
/// 1. They are the same function
/// 2. Every argument that is not a placeholder in the pattern fits that argument in the pattern
/// 3. Whatever expression is found in the place of a placeholder must be a fit with every other argument that has the same placeholder id
/// TODO - You need to handle expressions without arguments (i.e. variables) in a special way
/// 
/// TODO - You need to be using a reference to the same hashmap for all recursive function calls
fn shared_fits<'a>(self_expr: &'a Box<dyn Expression>, pattern: &Box<dyn Expression>) -> bool {
    // HashMap keeps track of the expression associated with a placeholder. If we ever find a mismatch, immediately return false.
    let mut placeholders: HashMap<u64, &Box<dyn Expression>> = HashMap::new();
    return shared_fits_helper(self_expr, pattern, &mut placeholders);
}
fn shared_fits_helper<'a>(self_expr: &'a Box<dyn Expression>, pattern: &Box<dyn Expression>, placeholders: &mut HashMap<u64, &'a Box<dyn Expression>>) -> bool {

    // Base Case - zero args (Value, Placeholder, and Variable)
    if pattern.args().len() == 0 {
        if pattern.id() == special_expression_id(SpecialExprType::Value) {
            // Values must be the same
            if let Some(casted_pattern) = pattern.as_any().downcast_ref::<Value>() {
                if let Some(casted_expr) = self_expr.as_any().downcast_ref::<Value>() {
                    if casted_pattern.val != casted_expr.val {
                        return false;
                    }
                }
            }
        } else if pattern.id() == special_expression_id(SpecialExprType::Placeholder) {
            if let Some(casted_placeholder) = pattern.as_any().downcast_ref::<Placeholder>() {
                // If it's a placeholder, check the HashMap
                // Sketch: Apparently the |ref| *ref closure fixed the problem of borrowing both mutably and immutably. dk why
                if let Some(placeholder_pattern) = placeholders.get(&casted_placeholder.placeholder_id).map(|reference| *reference) {
                    // If we've already seen this placeholder but the expressions don't match, return false
                    if !shared_fits_helper(self_expr, placeholder_pattern, placeholders) {
                        println!("false because placeholders don't match: {} != {}", self_expr, placeholder_pattern);
                        return false;
                    } else {
                        return true;
                    }
                } else {
                    // New placeholder, add to the HashMap
                    // TODO - plan of attack is to ignore this guy, silence all errors so you can try the & vs Box thing IN A NEW BRANCH
                    placeholders.insert(casted_placeholder.placeholder_id, self_expr);
                    println!("Placeholder pattern: {} := {}", pattern, self_expr);
                    println!("Total placeholders dict is now: {:?}", placeholders);
                    return true;
                }
            }
        } else if pattern.id() == special_expression_id(SpecialExprType::Variable) {
            if let Some(casted_pattern) = pattern.as_any().downcast_ref::<Variable>() {
                if let Some(casted_expr) = self_expr.as_any().downcast_ref::<Variable>() {
                    if casted_pattern.var_id != casted_expr.var_id {
                        return false;
                    }
                }
            }
        }
    }

    // Check that they are the same function
    if self_expr.id() != pattern.id() {
        println!("false because ids don't match: {} != {}", self_expr.id(), pattern.id());
        return false;
    }

    // Check that all args are a match
    // TODO - this function should first try to reduce as far as possible, which means you need a function to do that
    for i in 0..pattern.args().len() {
        let patt_arg = pattern.arg(i);
        let expr_arg = self_expr.arg(i);

        if !shared_fits_helper(expr_arg, patt_arg, placeholders) {
            println!("false because argument doesn't fit: {} != {}", expr_arg, patt_arg);
            return false;
        }
    }
    return true;
}

// MACROS

#[macro_export]
macro_rules! define_fn_struct_struct {
    ($ident:ident) => {
        struct $ident {
            args: Vec<Box<dyn Expression>>,
            dims: Vec<u128>,
            id: u64
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
            fn compute(&self, args: Vec<f64>) -> f64 {
                return $eval(args);
            }
            fn eval(&self, vars: &HashMap<u64, f64>) -> f64 {
                let mut evaluated = vec![];
                for arg in self.args() {
                    evaluated.push(arg.eval(vars));
                }
                return self.compute(evaluated);
            }
            fn id(&self) -> u64 {
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
macro_rules! make_id_constant {
    ($name:ident, $val:expr) => {
        paste! {
            const [<expr_id_for_ $name>]: u64 = $val;
        }
    };
    ($name:ident, $val:literal) => {
        paste! {
            const [<expr_id_for_ $name>]: u64 = $val;
        }
    };
}

// const id_hashtable: HashMap<String, u64> = HashMap::new();

fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}

#[macro_export]
macro_rules! define_fn_fn {
    ($ident:ident, $name:literal, $eval:expr) => {
        fn $ident(a: Box<dyn Expression>) -> Box<dyn Expression> {
            let dims = copy_vec(a.dims());
            let id = calculate_hash(&stringify!($ident));
            return Box::new($ident{args: vec![a], dims: dims, id})
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
            let id = calculate_hash(&stringify!($ident));
            return Box::new($ident{args: vec![lhs, rhs], dims: dims, id})
        }

        impl ops::$ident<f64> for &Box<dyn Expression> {
            type Output = Box<dyn Expression>;
            
            fn $op_ident(self, _rhs: f64) -> Box<dyn Expression> {
                return $ident(self, &Value(_rhs));
            }
        }
        
        impl ops::$ident<&Box<dyn Expression>> for f64 {
            type Output = Box<dyn Expression>;
            
            fn $op_ident(self, _rhs: Box<dyn Expression>) -> Box<dyn Expression> {
                return $ident(Value(self), _rhs);
            }
        }

        impl ops::$ident<u128> for &Box<dyn Expression> {
            type Output = Box<dyn Expression>;
            
            fn $op_ident(self, _rhs: u128) -> Box<dyn Expression> {
                return $ident(self, &Value(_rhs as f64));
            }
        }
        
        impl ops::$ident<&Box<dyn Expression>> for u128 {
            type Output = Box<dyn Expression>;
            
            fn $op_ident(self, _rhs: &Box<dyn Expression>) -> Box<dyn Expression> {
                return $ident(&Value(self as f64), _rhs);
            }
        }

        impl ops::$ident<&Box<dyn Expression>> for &Box<dyn Expression> {
            type Output = Box<dyn Expression>;
            
            fn $op_ident(self, _rhs: &Box<dyn Expression>) -> Box<dyn Expression> {
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

// STANDARD FUNCTIONS
define_unary_f64_fn!(Cos, "Cos", cos);
define_unary_f64_fn!(Sin, "Sin", sin);
define_unary_f64_fn!(Sqrt, "Sqrt", sqrt);
define_unary_f64_fn!(Ln, "Ln", ln);
define_unary_f64_fn!(Log2, "Log2", log2);
define_unary_f64_fn!(Log10, "Log10", log10);

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
    id: u64
}

fn Value(val: f64) -> Box<dyn Expression> {
    let id = calculate_hash(&"Value");
    return Box::new(Value {args: vec![], dims: vec![1], val, id})
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO - Also same below for Debug
        return write!(f, "{}", self.val);
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "{}", self.val);
    }
}

// CURRENT - ly in the middle of making all args pointers so you can implement binary ops for references, so you can pass things around easier.
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
    fn compute(&self, _args: Vec<f64>) -> f64 {
        return self.val;
    }
    fn eval(&self, vars: &HashMap<u64, f64>) -> f64 {
        let mut evaluated = vec![];
        for arg in self.args() {
            evaluated.push(arg.eval(vars));
        }
        return self.compute(evaluated);
    }
    fn id(&self) -> u64 {
        return self.id
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}


struct Variable {
    args: Vec<Box<dyn Expression>>,
    dims: Vec<u128>,
    id: u64,
    var_id: u64,
    symbol: String
}

static mut VARIABLE_ID_COUNTER: u64 = 0;

fn get_next_var_id() -> u64 {
    unsafe {
        VARIABLE_ID_COUNTER += 1;
        return VARIABLE_ID_COUNTER
    }
    
}


fn Variable(symbol: &str) -> Box<dyn Expression> {
    let id = calculate_hash(&"Variable");
    return Box::new(Variable {args: vec![], dims: vec![1], symbol: symbol.to_string(), var_id: get_next_var_id(), id})
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
    fn compute(&self, _args: Vec<f64>) -> f64 {
        return 0.0; // TODO! This is a special situation. You'll actually throw an error (Result::Err) here
        // because compute shouldn't ever be called on a Variable.
    }
    fn id(&self) -> u64 {
        return self.id
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn eval(&self, vars: &HashMap<u64, f64>) -> f64 {
        return vars[&self.var_id];
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
    id: u64,
    placeholder_id: u64,
}

static mut PLACEHOLDER_ID_COUNTER: u64 = 0;

fn Placeholder(placeholder_id: u64) -> Box<dyn Expression> {
    unsafe {
        PLACEHOLDER_ID_COUNTER += 1;
    }
    let id = calculate_hash(&"Placeholder");
    return Box::new(Placeholder {args: vec![], dims: vec![1], placeholder_id, id})
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
    fn compute(&self, _args: Vec<f64>) -> f64 {
        panic!("Can't compute placeholder");
    }
    fn eval(&self, _vars: &HashMap<u64, f64>) -> f64 {
        panic!("Can't eval placeholder");
    }
    fn id(&self) -> u64 {
        return self.id
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

fn main() {
    // let x = Variable("x");
    // let expr1 = x + 1;
    
    let mut line: String = String::new();
    loop {
        println!(">> ");
        stdin().read_line(&mut line).unwrap();
        println!("{}", line);
    }

    // let theta = Value(std::f64::consts::FRAC_PI_3);
    // let s = Sin(theta);
    // let a = Value(5.0);
    // let b = Value(6.0);
    // let t = a + b;
    // // let t = Add(a, b);
    // println!("{} = {}", s, s.eval(&HashMap::new()));
    // println!("{} = {}", t, t.eval(&HashMap::new()));
    // // let ss = Sin(theta); // ??? Tough because Box doesn't implement copy

    // let c = Variable("c");
    // let d = Variable("d");
    // let expr3 = (2 * c + d - 10) / 2;
    // println!("{} = {}", expr3, expr3.eval(&HashMap::from([(1, 4.0), (2, 5.0)])));

    // let co = Cos(Value(0.0));
    // println!("{} = {}", co, co.eval(&HashMap::new()));

    // // Try again after with variables instead of values
    // // Should you just be using variables instead of placeholders??
    
    // // The below is preferable, but this is the same problem as copyable expression pointers.
    // // let p = Placeholder();
    // // let pyid = Cos(p) * Cos(p) + Sin(p) * Sin(p);
    // let x = Variable("x");
    // let expr5 = x + x;
    // let expr4 = Cos(Value(1.0)) * Cos(Value(0.5) + Value(0.5)) + Sin(Value(1.0)) * Sin(Value(1.0));
    // let pyid = Cos(Placeholder(1)) * Cos(Placeholder(1)) + Sin(Placeholder(1)) * Sin(Placeholder(1));

    // println!("{} == {} ??? {}", expr4, pyid, shared_fits(&expr4, &pyid));

    //pyid matching with variables (see TODO in the shared_fit docstring), and then start working on sub, then look into procedural macros


}


// MACRO TREE
//
// Granular customization by specifying exactly which portion you want to specify, and it calculates which other macros to run
// For example, if I only want to customize define_fn_struct_impl_expr_evaluate, then I say so and the tree does all of define_fn_fn, define_fn_struct_struct, define_fn_struct_impl_expr_{siblings}, and then finally this one in its correct order.
// Or just better to define structs with defaults??
// struct MacroTree {
//     subs: Vec<MacroTree>,
//     // run: How do I represent the type of a macro???
// }

// TODO
// * Swap out ndarray for f64
// * macro for general arity
// * Copyable Expression pointers? Can you fix this by just making expressions Copyable?
//   * Macro environment for defining expressions, then could do whatever you want, but why make it in rust at this point?
//   * NewExpr macro NewExpr(y, x^2 + 5*x + 4) ($ident:ident, $expr:expr) => {...} that automatically clones when needed


// * dims is unecessary?

/// * Useful Algorithms
///     * Polynomial Multiplication - DFT
///     * Integration - Risch Algorithm
///     * Factorization - Cantor-Zassenhaus
/// * https://fredrikj.net/blog/2022/04/things-i-would-like-to-see-in-a-computer-algebra-system/
/// * A cool way to allow other algebras or types of objects: http://fperez.org/py4science/2009_siam_cse/CSE09_pearu_slides.pdf, but an alternative that I like even more is to build around a type system (e.g. a set or a number would be different types. Even an algebra might be a type that you could manipulate.) Note that commutation is an important concept that will affect some of your algorithms for expression comparison.
/// * Here's how some of the famous CAS systems optimize:
///     * Macsyma: http://people.eecs.berkeley.edu/~fateman/papers/Fateman-Salz_Simplifier_Paper.pdf
///     * Mathematica: https://dl.acm.org/doi/abs/10.5555/129711
///     * Maple: https://d1wqtxts1xzle7.cloudfront.net/31139001/1984-muc-design-with-cover-page-v2.pdf?Expires=1654008182&Signature=I1CQzftULHyHsojKnzoidOmPc29RN3Ur1QrlTTsB1rpzgmyLU2xU3KZjwei8k15MtxSvQ-Z-MLVqMMbFAphvZE5Qe5~-XtqS3wVnc-CpBpgg0aA4YfMOCAXmw5Z0Ng2EVu~1QJHwUI5vHMYbWS5IYj1Bk5g7zm4jdhP25iN3IGv2EGiFf6nIIbeOZho2HWoTTJitASRC3ogT3Tak~U5g2~USe7--CUEL4xOJbuEncIavg4Es-MKERaJHSvGiLmHFUPWzYZYdwS2J~wS-dFC9tmBUipW~qNwAeUwlkm6VFKV1iBf3ZRDXdn6pxPLxf3v-PDOI6i7YT0SBRclnvu8PVg__&Key-Pair-Id=APKAJLOHF5GGSLRBV4ZA
const _: i8 = 0;
