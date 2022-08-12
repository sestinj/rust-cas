trait Expression {
    fn args(&self) -> Vec<&dyn Expression>;
    fn arg(&self, idx: usize) -> &dyn Expression;
    fn compute(&self, args: Vec<f64>) -> f64;
}

struct Value {
    args: Vec<&dyn Expression>,
    val: f64,
    id: u64
}

struct Add {
    args: Vec<&dyn Expression>,
    id: u64
}

fn main() {
    let a = Value{args: vec![], val: 5.0, id: 0};
    let b = Value{args: vec![], val: 10.0, id: 1};
    let expr1 = Add{args: vec![a, b], id: 2};
}



// 1. Try &dyn references
// 2. Try storing your own references, just the ids of the arguments and keep all actual values in some big table somewhere. But is this slower?