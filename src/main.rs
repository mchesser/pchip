use std::io::File;
use std::os;

mod parser;

fn main() {
    let args = os::args();
    if args.len() != 2 {
        println!("Invalid usage");
        return;
    }
    let input = match File::open(&Path::new(args[1])) {
        Some(mut f) => f.read_to_str(),
        None => { println!("Error reading file"); return; }
    };
    let code = parser::parse(input);
    let mut output = File::create(&Path::new("program.ch8"));
    output.write(code);
}
