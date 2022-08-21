mod cas_env;
use cas_env::{
    env,
    init_env,
    Expression
};

fn main() {
    init_env();

    let a = &env.value(5.0);
    let b = &env.value(10.0);
    
    let c = &env.add(a, b);
    let d = env.add(a, c);

    print!("{}", d);
}
// The problem now is that you can't return a reference to an expression from a function.
// So you will always have to write the ampersand as above...but there are still errors? Not sure what's up