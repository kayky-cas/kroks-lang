use std::process::exit;

#[derive(Debug)]
enum Token {
    Integer(i64),
    Plus,
}

struct Lexer<'a> {
    buffer: &'a [char],
}

impl<'a> Lexer<'a> {
    fn new(buffer: &'a [char]) -> Self {
        Lexer { buffer }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let mut buffer = self.buffer;

        while !buffer.is_empty() && buffer[0].is_whitespace() {
            buffer = &buffer[1..];
        }

        if buffer.is_empty() {
            return None;
        }

        let c = buffer[0];

        match c {
            '0'..='9' => {
                let mut number = String::new();

                while !buffer.is_empty() && buffer[0].is_digit(10) {
                    number.push(buffer[0]);
                    buffer = &buffer[1..];
                }

                self.buffer = buffer;

                Some(Token::Integer(number.parse::<i64>().unwrap()))
            }
            '+' => {
                self.buffer = &buffer[1..];
                Some(Token::Plus)
            }
            _ => None,
        }
    }
}

struct Compiler {
    stack: Vec<Token>,
}

impl Compiler {
    fn compile(&mut self) {
        todo!()
    }
}

struct Simulator {
    stack: Vec<Token>,
}

impl Simulator {
    fn run(&mut self) {
        todo!()
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
            let mut compiler = Compiler { stack };
            compiler.compile();
        }
        "sim" => {
            let mut simulator = Simulator { stack };
            simulator.run();
        }
        _ => {
            usage(&args[0]);
        }
    }
}
