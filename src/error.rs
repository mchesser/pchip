use std::fmt;

#[derive(Debug, Copy, Clone)]
pub struct InputSpan {
    pub start: InputPos,
    pub end: InputPos,
}

impl fmt::Display for InputSpan {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\n")
    }
}

impl InputSpan {
    pub fn new(start: InputPos, end: InputPos) -> InputSpan {
        InputSpan {
            start: start,
            end: end,
        }
    }

    pub fn invalid() -> InputSpan {
        InputSpan {
            start: InputPos::start(),
            end: InputPos::start(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct InputPos {
    pub col: usize,
    pub line: usize,
}

impl InputPos {
    pub fn start() -> InputPos {
        InputPos {
            col: 0,
            line: 1,
        }
    }
}

pub struct Logger<'a> {
    lines: Vec<&'a str>,
    print_span: bool,
}

impl<'a> Logger<'a> {
    pub fn new(input: &'a str, print_span: bool) -> Logger<'a> {
        Logger {
            lines: input.lines().collect(),
            print_span: print_span,
        }
    }

    pub fn report_error(&self, message: String, input_span: InputSpan) {
        println!("unknown.pcp:{}:{}: {}:{} {}", input_span.start.line, input_span.start.col,
            input_span.end.line, input_span.end.col, message);

        if self.print_span {
            for i in input_span.start.line..(input_span.end.line + 1) {
                println!("{}", self.lines[i - 1]);
            }
            println!("");
        }
    }
}
