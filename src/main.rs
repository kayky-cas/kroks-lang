use std::{
    fmt::{self, Display, Formatter},
    process::exit,
};

struct TokenPath(usize, usize);

impl Display for TokenPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.0, self.1)
    }
}

enum Token {
    Integer(TokenPath, i64),
    Plus(TokenPath),
}

struct Lexer<'a> {
    buffer: &'a [char],
    line: usize,
    column: usize,
}

impl<'a> Lexer<'a> {
    fn new(buffer: &'a [char]) -> Self {
        Lexer {
            buffer,
            line: 1,
            column: 1,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let mut buffer = self.buffer;

        while !buffer.is_empty() && buffer[0].is_whitespace() {
            buffer = &buffer[1..];
            self.column += 1;
            if buffer[0] == '\n' {
                self.line += 1;
                self.column = 1;
            }
        }

        if buffer.is_empty() {
            return None;
        }

        let c = buffer[0];
        let token_path = TokenPath(self.line, self.column);

        match c {
            '0'..='9' => {
                let mut number = String::new();

                while !buffer.is_empty() && buffer[0].is_digit(10) {
                    number.push(buffer[0]);
                    buffer = &buffer[1..];
                    self.column += 1;
                    if buffer[0] == '\n' {
                        self.line += 1;
                        self.column = 1;
                    }
                }

                self.buffer = buffer;

                Some(Token::Integer(token_path, number.parse::<i64>().unwrap()))
            }
            '+' => {
                self.buffer = &buffer[1..];
                Some(Token::Plus(token_path))
            }
            _ => None,
        }
    }
}

struct Compiler {
    program: Vec<Token>,
}

impl Compiler {
    fn compile(&mut self) {
        todo!()
    }
}

struct Simulator {
    program: Vec<Token>,
}

impl Simulator {
    fn run(&mut self) {
        let mut stack = Vec::new();

        while let Some(token) = self.program.pop() {
            match token {
                Token::Integer(_, n) => {
                    stack.push(n);
                }
                Token::Plus(path) => {
                    let (a, b) = match (stack.pop(), stack.pop()) {
                        (Some(a), Some(b)) => (a, b),
                        _ => {
                            println!("{}: stack underflow", path);
                            exit(1);
                        }
                    };

                    stack.push(a + b);
                }
            }
        }
    }
}

fn usage(program: &str) {
    println!("Usage: {} [com | sim] <file>", program);
    exit(1);
}

fn main() {
    let args = std::env::args().collect::<Vec<String>>();

    if args.len() < 3 {
        usage(&args[0]);
    }

    let buffer = std::fs::read_to_string(&args[1]).unwrap();
    let buffer = buffer.chars().collect::<Vec<char>>();

    let lexer = Lexer::new(&buffer);

    let mut stack: Vec<Token> = lexer.into_iter().collect();

    match &args[1][..] {
        "com" => {
            let mut compiler = Compiler { program: stack };
            compiler.compile();
        }
        "sim" => {
            let mut simulator = Simulator { program: stack };
            simulator.run();
        }
        _ => {
            usage(&args[0]);
        }
    }
}
