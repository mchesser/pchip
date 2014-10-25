#![feature(tuple_indexing)]

use std::io::File;
use std::os;

use std::cmp::min;

use parser::parse;
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

    let logger = Logger::new(input.as_slice(), true);
    let program = parse(Lexer::new(input.as_slice()), &logger);
    let code = codegen(program, &logger, true);

    let mut space = 0;
    let mut program_string = String::new();
    for inst in code.into_iter() {
        match inst {
            dlx::asm::Label(label) => {
                if space != 0 {
                    program_string.push('\n');
                    space = 0;
                }
                program_string.push_str(label.as_slice());
                space += label.len();
            },
            dlx::asm::RawAsm(data) => {
                if space != 0 {
                    program_string.push('\n');
                    space = 0;
                }
                program_string.push_str(data.as_slice());
                program_string.push('\n');
            },
            other => {
                let data = other.into_string();
                program_string.grow(8 - min(7, space), ' ');
                program_string.push_str(data.as_slice());
                program_string.push('\n');
                space = 0;
            }
        }
    }

    println!("{}", program_string);
}
