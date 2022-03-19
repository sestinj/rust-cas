use std::ops;
use std::fmt;
use std::collections::HashMap;
use seq_macro::seq;
use std::panic;

enum Domain {
    R,
    C,
    N,
    Z,
    Zp,
    Zm,
    Zmod(u128),
    Empty
}
// impl union, inersection, etc... of domains

struct Requirement {

}

// struct Variable<T> {
//     value: T,
//     domain: Domain
// }

struct Comparison {
    // I want to be able to specify a set of equations and or inequalities in order to solve for variables
    // In fact, I think even a comparison should be the same thing as an expression, because then you can work with booleans
}

// enum Simplifier {
//     // I want to be able to add in new pattern matchers that can solve and simplify in different situations.
//     // I should be able to state them in the form of two expressions, representing an equivalence.
//     // The entire algorithm of simplifying should in fact be a search over these equivalences.
//     // So I think that using the comparison struct makes more sense above. But keeping this here for now.
//     PythagoreanTrigIdentity
// }

// impl Simplifier {
//     fn simplify(&self, expr: Expression) -> Expression {
//         match self {

//         }
//     }
// }

////////////////////////
/// 
/// 

// Can I use a generic T instead of f64 here, or even a Domain, without always having to write Expression<T>? But Domains shouldn't be tied to the entire expression, you might want to interact between integers and real numbers for example. Should be decided at the variable level.
enum Op {
    Add,
    Identity(f64),
    Variable(u128), // Variable identifier,
    Placeholder(u128), // Used for pattern matching, uses u128 as identifier
    Sin,
    Cos,
    Pow
}

struct Simplifier(Expression, Expression, String);

impl Simplifier {
    fn rev(&self) -> Simplifier {
        return Simplifier(self.1, self.0, self.2);
    }
}

const Given: Simplifier = Simplifier(Identity(1.0), Identity(1.0), "Given".to_string());

// ^ is a bit operator, so either overload or impl a .pow(Expression) for Expression.
// const SIMPLIFIERS: Vec<Simplifier> = vec![
//     Simplifier(Pow(Sin(Op::Placeholder(0)), 2.0) + Pow(Cos(Op::Placeholder(1)), 2.0), Identity(1.0), "Pythagorean Trig Identity".to_string())
// ];
// Would you ever want to use a macro so you can set two expressions equal with walrus syntax or something?

fn check_match(expr: Expression, simplifier: Simplifier) -> bool {
    // You might later on want to return more than just a bool-perhaps a correspondence between variable ids in expression and simplifier
    panic!("check_match not yet implemented.");
    return true;
}

fn apply_simplifier(expr: Expression, simplifier: Simplifier) -> Expression {
    panic!("This goes hand-in-hand with the above.");
    return expr;
}

// fn simplify(expr: Expression, identities: Vec<Simplifier>) -> Expression {
//     /// SIMPLIFIER Algorithm description:
//     /// 1) Find all matching patterns for the current
//     ///     * Will be interesting to consider how this can be sped up by basically
//     ///         shortcircuiting the matcher. Could do a lot of really clever stuff here,
//     ///         especially if you consider your knowledge of generally what Simplifiers are available
//     /// 2) Do a depth search on the patterns keeping track of the best final expressions that you get
//     ///     * How do you qualify "best"? In most cases, this is just the number of free variables remaining
//     ///         or the length of the expression. But might be different metrics useful at different times.
//     /// * If certain patterns of simplifications are common, should this be added as a single Simplifier,
//     ///     or should you train GPT-3 to learn sequences of applying Simplifiers?
    
//     // This stack is a list of the steps, each giving the simplifier (or GIVEN) used and the expression at the end of the step
//     // This is a bad representation though because you don't want to recopy the path each time...should have a pointer to parent step
//     struct Step {
//         resulting_expr: Expression,
//         simplifier: Simplifier,
//         parent: Box<Step> // How do you bottom out ?!??!?!???!!? Option probably
//     }

//     let stack: Vec<(Vec<Expression>, Vec<Simplifier>)> = vec![Step {resulting_expr: expr, simplifier: Given, parent: None}];
//     for simplifier in SIMPLIFIERS {
//         if check_match(expr, simplifier) {
//             stack.push(apply_simplifier(expr, simplifier: Simplifier))
//         }
//         /// Should we check the reverse??? (simplifier.rev()) If we do, then the tree is going to infinitely expand, but within
//         /// reason there are situations where some creative deviation is needed.
//         /// Save this for later, but I think you'll want to set up some rules about when this is done (not just "creatively")
//         /// and also maybe only allow one or two of these in a row.
//     }
// }

// An expression is a tree where args are the children
struct Expression {
    args: Vec<Expression>,
    op: Op,
    val: Option<f64>,
    // domain: Domain
    // If this value ever exists, the other parts of the struct do not matter. They should be treated as non-existent. This isn't a super obvious behavior though. Still looking for a better implementation. For the moment, trying to use Identity Operation whenever this is the case.
}

impl Expression {
    fn identity(val: f64) -> Expression {
        return Expression {args: vec![], op: Op::Identity(val), val: Some(val)};
    }

    // I want to be able to give multiple different sets of values to evaluate the same expression.
    // This requires the concept of variables. Identity is not that.
    // This function should be able to return an expression if it isn't given all variable values.
    // HOW do I treat f64 and Expression as a single union type???

    /// Recursively Evaluates (S) Expression as far as possible
    /// # Arguments
    /// * `values` - A map of variable identifiers (u128) to their values (f64)
    fn evaluate(&self, values: HashMap<u128, f64>) -> Expression {
        let partially_evaluated_args: Vec<Expression> = vec![];
        let args: Vec<f64> = vec![];
        let incomplete = false;

        for arg in self.args {
            let result = arg.evaluate(values);
            match result.val {
                Some(x) => {
                    args.push(x);
                    partially_evaluated_args.push(result);
                },
                None => {
                    incomplete = true;
                    partially_evaluated_args.push(result);
                }
            }
        }
        // NOTE - Expression.val probably unecessary because you have the Identity(val) right?? This helps consistency.
        if incomplete {
            // If any of the arguments aren't evaluated fully, you shouldn't further evaluate this expression (use simplifiers though)
            return Expression{args: partially_evaluated_args, op: self.op, val: None};
        }
        
        // Variables are special in that they are the only, for now, Op that has no arg and could be unevaluated.
        match self.op {
            Op::Variable(val) => {
                if values.contains_key(&val) {
                    return Expression::identity(values[&val]);
                } else {
                    return Expression{..*self};
                }
            }
        }

        // If all sub-expressions are evaluated fully, we know we can fully evaluate this one.
        let val = match self.op {
            Op::Identity(val) => val,
            Op::Add => args[0] + args[1],
            Op::Sin => args[0].sin(),
            Op::Cos => args[0].cos(),
            Op::Pow => args[0].powf(args[1]),
            Op::Placeholder(id) => 0.0,
            Op::Variable(id) => panic!("This shouldn't be possible"),
        };

        return Expression::identity(val);
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.op {
            Op::Add => write!(f, "{} + {}", self.args[0], self.args[1]),
            Op::Identity(val) => write!(f, "{}", val),
            Op::Pow => write!(f, "{}^{}", self.args[0], self.args[1]),
            Op::Sin => write!(f, "sin({})", self.args[0]),
            Op::Cos => write!(f, "cos({})", self.args[0])
        }
    }
}

impl ops::Add<Expression> for Expression {
    type Output = Expression;
    
    fn add(self, _rhs: Expression) -> Expression {
        return Add(self, _rhs);
    }
}


// Can I write macros for these things? Don't feel like I should be writing both the forward and backward versions
impl ops::Add<f64> for Expression {
    type Output = Expression;
    
    fn add(self, _rhs: f64) -> Expression {
        return Add(self, Identity(_rhs));
    }
}

impl ops::Add<Expression> for f64 {
    type Output = Expression;
    
    fn add(self, _rhs: Expression) -> Expression {
        return Add(Identity(self), _rhs);
    }
}

// I shouldn't ever have to rewrite this. How can I make the enum callable or something?
// With macros!!!
// Then can I automatically call this macro in a loop?? :O
// #[macro_export]
// macro_rules! define_fn {
//     ( $x:ident, $n:literal ) => {
//         fn $x(seq!{ N in 0..$n { aN: Expression }}) -> Expression {
//             return Expression {args: vec![seq!{N in 0..$n { aN }}], op: Op::$x};
//         }
//     };
// }

// A meta-macro
#[macro_export]
macro_rules! define_fn {
    ($name:ident, $n:literal) => {

    }
}

#[macro_export]
macro_rules! Sin {
    ($a:literal) => {
        return Expression {args: vec![$a], op: Op::Sin, val: None};
    };
}

// define_fn!(Sin, 1);
// define_fn!(Cos, 1);
// define_fn!(Pow, 2);

fn Add(a: Expression, b: Expression) -> Expression {
    return Expression {args: vec![a, b], op: Op::Add, val: None};
}

fn Pow(a: Expression, b: Expression) -> Expression {
    return Expression {args: vec![a, b], op: Op::Add, val: None};
}

fn Sin(a: Expression) -> Expression {
    return Expression {args: vec![a], op: Op::Sin, val: None};
}

fn Cos(a: Expression) -> Expression {
    return Expression {args: vec![a], op: Op::Cos, val: None};
}

fn Identity(a: f64) -> Expression {
    return Expression{args: vec![], op: Op::Identity(a), val: Some(a)};
}

fn Variable(id: u128) -> Expression {
    return Expression{args: vec![], op: Op::Variable(id), val: None};
}

fn main() {
    let x = Identity(5.0);
    let y = Identity(2.0);
    let expression = 2.0 + x + y;

    println!("Expression: {}", expression);
    println!("Evaluated: {}", expression.evaluate(HashMap::new()));

    let z = Identity(0.0);
    let a = Identity(3.0);
    let b = Identity(5.0);
    let expr2 = Pow(a, b) + Sin(z) + 2.0;
    // let expr2 = Pow(x, 2.0); This is a problem :(

    println!("expr2: {} = {}", expr2, expr2.evaluate(HashMap::new()));


    let l = Variable(0); // Can you just autoincrement? I think this is only needed by the user in the case of placeholders
    let m = Variable(1);
    let n = Variable(2);

    let expr3 = Pow(l, m) + Sin(n);

    println!("expr3: {} = {}", expr3, expr3.evaluate(HashMap::from([(0, 5.0), (1, 3.0), (2, std::f64::consts::FRAC_PI_2)])));
}


// I want to be able to write my expressions in rust so that I can assemble them programmatically
// I want to be able to evaluate, simplify, and solve expressions and sets of equations.
// I want to be able to extend the function library.
// I want everything to be automatically differentiable.
// I want to be able to convert in both directions between Rust and LaTex.

// I want to be able to symbolically represent a neural network: either actually represent all of the addition and multiplication operations as one big equation, or to have a neural network as an Operation itself, completely atomic such that it is composable and analyzable.