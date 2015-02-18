#![feature(box_syntax, core, io, path, hash, collections, env, std_misc)]

use std::old_io::File;

use std::cmp::min;
use std::iter;

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
    let args: Vec<_> = std::env::args().collect();
    if args.len() != 2 {
        println!("Invalid usage");
        return;
    }
    let mut file = match File::open(&Path::new(args[1].clone())) {
        Ok(f) => f,
        Err(e) => panic!("Error opening file: {}", e)
    };
    let input = match file.read_to_string() {
        Ok(input) => input,
        Err(e)  => panic!("Error reading file: {}", e)
    };

    let logger = Logger::new(&input, true);
    let program = parse(Lexer::new(&input), &logger);
    let code = codegen(program, &logger, true);

    let mut space = 0;
    let mut program_string = String::new();
    for inst in code {
        match inst {
            dlx::asm::Label(label) => {
                if space != 0 {
                    program_string.push('\n');
                    space = 0;
                }
                program_string.push_str(&label);
                space += label.len();
            },
            dlx::asm::RawAsm(data) => {
                if space != 0 {
                    program_string.push('\n');
                    space = 0;
                }
                program_string.push_str(&data);
                program_string.push('\n');
            },
            other => {
                let data = other.to_string();
                program_string.extend(iter::repeat(' ').take(8 - min(7, space)));
                program_string.push_str(&data);
                program_string.push('\n');
                space = 0;
            }
        }
    }

    println!("{}", program_string);
}
