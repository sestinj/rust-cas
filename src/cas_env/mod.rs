use std::{
    collections::{
        HashMap
    },
    fmt
};

struct Operator {
    raw_calc: fn(Vec<f64>) -> f64,
    id: u64
}

impl Operator {
    fn lowercase(&self) -> String {
        format!("{}", self.id)
    }
}

// #[derive(Clone)]
struct CasEnv {
    operators: HashMap<u64, Operator>,
    last_op_id: u64,
    last_var_id: u64
}

impl CasEnv {
    fn reg_op(&mut self, op: Operator) {
        self.last_op_id += 1;
        self.operators.insert(self.last_op_id, op);
    }
    fn get_op(&self, op_id: u64) -> Operator {
        return *self.operators.get(&op_id).unwrap();
    }
    fn get_last_var_id(&mut self) -> u64 {
        self.last_var_id += 1;
        return self.last_var_id;
    }
}

pub struct LazyVal {
    val: f64,
    lazy: bool
}

pub struct Expression<'a> {
    args: Vec<&'a Expression<'a>>,
    op: u64,
    id: u64,
    val: LazyVal
}

impl<'a> fmt::Display for Expression<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO - Also same below for Debug
        return write!(f, "{}", self.val.val);
    }
}

impl<'a> fmt::Debug for Expression<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "{}", self.val.val);
    }
}

// We create a baseline environment with the standard library of operators, but then initialization creates a clone which can be forked to register new operators
pub const env: CasEnv = CasEnv {
    operators: HashMap::new(),
    last_op_id: 0,
    last_var_id: 0
};

// COMMON LIBRARY OPERATORS //

// This way the functions are always attached to an environment
impl CasEnv {
    pub fn add<'a>(&self, a: &'a Expression, b: &'a Expression) -> Expression<'a> {
        let id = self.get_last_var_id();
        return Expression{
            args: vec![a, b],
            op: 1,
            id: id,
            val: LazyVal{
                val: 0.0,
                lazy: true
            }
        };
    }

    pub fn value(&self, v: f64) -> Expression{
        return Expression{
            args: vec![],
            op: 2,
            id: self.get_last_var_id(),
            val: LazyVal{
                val: v,
                lazy: false
            }
        };
    }
}

// impl<'a> ops::Add<&'a Expression<'a>> for &'a Expression<'a> {
//     type Output = &'a Expression<'a>;
//     fn add(self, rhs: &'a Expression<'a>) -> &'a Expression<'a> {
//         return env.add<'a>(self, rhs)
//     }
// }

pub fn init_env() {
    // Register the standard operators
    env.reg_op(Operator {
        id: 1,
        raw_calc: |args| {
            return args[0] + args[1];
        }
    });

    env.reg_op(Operator {
        id: 2,
        raw_calc: |args| {
            return 0.0;
        }
    })
    // return env.clone();
}

// fn new_env() -> CasEnv {
//     return env.clone();
// }

// Macro to make an environment the default environment (so you don't have to prefix with env.)
#[macro_export]
macro_rules! default_env {
    ($env:ident, $op:ident) => {
        fn $op(a: &Expression, b: &Expression) -> &Expression {
            return $ident.$op(a, b);
        }
    }
}