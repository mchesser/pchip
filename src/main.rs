mod parser;

fn main() {
    let input = &"let a = 10\nlet b = a + 5";
    println!("{:?}", parser::Parser::evaluate(input));
}
