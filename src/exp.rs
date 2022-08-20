trait Expression: fmt::Display + fmt::Error {
    fn eval(&self) -> Result<i32, Box<dyn Error>>;
}

struct Test {
    args: Box<Vec<Box<dyn Expression>>>;
}

impl Expression for Test {
    fn eval(&self) -> Result<i32, box<dyn Error>> {
        return Ok(5);
    }
}

fn add(a: Box<dyn Expression>, b: Box<dyn Expression>) -> i32 {

}

// Linked list instead of vector
// Pointer to vector

fn main() {
    let a = Test { args: Box::new(vec![Box::new(Test {})]) };
    let b = Test { args: Box::new(vec![Box::new(Test {})]) };
    let c = add(Box::new(a), Box::new(b));
    let d = add(Box::new(a), Box::new(b));
}