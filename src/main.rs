use std::io::File;
use std::os;

use parser::Parser;
use lexer::Lexer;
use error::Logger;
use dlx::codegen::codegen;

mod parser;
mod dlx;
mod lexer;
mod ast;
mod error;

fn main() {
    let args = os::args();
    if args.len() != 2 {
        println!("Invalid usage");
        return;
    }
    let mut file = match File::open(&Path::new(args[1].clone())) {
        Ok(f) => f,
        Err(err) => fail!("Error opening file: {}", err)
    };
    let input = match file.read_to_string() {
        Ok(input) => input,
        Err(err)  => fail!("Error reading file: {}", err)
    };
    let mut parser = Parser::new(Lexer::new(input.as_slice()), Logger::new(input.as_slice(), false));
    let program = parser.parse();
    println!("{}", program);

    let code = codegen(program);

    let mut space = 0;
    let mut program_string = String::new();
    for inst in code.move_iter() {
        match inst {
            dlx::asm::Label(label) => {
                program_string.push_str(label.as_slice());
                space += label.len();
            },
            other => {
                let data = other.into_string();
                program_string.grow(8 - space, ' ');
                program_string.push_str(data.as_slice());
                program_string.push_char('\n');
                space = 0;
            }
        }
    }

    println!("{}", program_string);
}
