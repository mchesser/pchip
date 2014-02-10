use std::io::File;
use std::os;

mod parser;

fn main() {
    let args = os::args();
    if args.len() != 2 {
        println!("Invalid usage");
        return;
    }
    let mut file = match File::open(&Path::new(args[1])) {
        Ok(f)    => f,
        Err(err) => fail!("Error openning file: {:?}", err)
    };
    let input = match file.read_to_str() {
        Ok(input) => input,
        Err(err)  => fail!("Error reading file: {:?}", err)
    };
    let code = parser::parse(input);
    let mut output = File::create(&Path::new("program.ch8"));

    match output.write(code) {
        Ok(_)    => {},
        Err(err) => println!("Failed to write to file: {:?}", err)
    }
}
