use std::io::File;
use std::os;

mod parser;
mod lexer;
mod ast;
mod trans;
mod asm;
mod chip8;

fn main() {
    let args = os::args();
    if args.len() != 2 {
        println!("Invalid usage");
        return;
    }
    let mut file = match File::open(&Path::new(args.get(1).clone())) {
        Ok(f)    => f,
        Err(err) => fail!("Error opening file: {}", err.desc)
    };
    let input = match file.read_to_str() {
        Ok(input) => input,
        Err(err)  => fail!("Error reading file: {}", err.desc)
    };
    let code = parser::parse(input.as_slice());
    let mut output = File::create(&Path::new("program.ch8"));

    match output.write(code.as_slice()) {
        Ok(_)    => {},
        Err(err) => println!("Failed to write to file: {}", err.desc)
    }
}
