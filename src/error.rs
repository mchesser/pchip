#[deriving(Show, Clone)]
pub struct InputSpan {
    pub start: InputPos,
    pub end: InputPos,
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

#[deriving(Show, Clone)]
pub struct InputPos {
    pub col: uint,
    pub line: uint,
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
    input: &'a str,
    print_span: bool,
}

impl<'a> Logger<'a> {
    pub fn new(input: &str, print_span: bool) -> Logger {
        Logger {
            input: input,
            print_span: print_span,
        }
    }

    pub fn report_error(&self, message: String, input_span: InputSpan) {
        println!("Error {}:{} - {}:{} : {}", input_span.start.line, input_span.start.col,
            input_span.end.line, input_span.end.col, message);
    }
}
