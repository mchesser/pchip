use lexer::Lexer;

mod lexer;

fn main() {
    let input = &"let a = 10\nfor i == 30\nlet b = (1)";
    let mut lexer = Lexer::new(input);
    for token in lexer {
        println!("{:?}", token);
    }
}