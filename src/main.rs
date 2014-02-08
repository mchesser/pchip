use std::io::File;

mod parser;

fn main() {
    let input =
r#"{
let x = 0;
loop {
    __clear();
    __draw_pos(x, 5);
    __get_font(3);
    __draw(5);
    x = x + 1;
    __key_wait(0);
}
}"#;

    let code = parser::parse(input);
    for &op in code.iter() {
        print!("{:02x}", op);
    }
    println!("");

    let mut file = File::create(&Path::new("a.out"));
    file.write(code);
}
