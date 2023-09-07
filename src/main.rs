use std::{
    fmt::{self, Display, Formatter},
    io::BufWriter,
    process::exit,
};

enum KroksMode {
    Compile,
    Simulate,
}

enum KroksSimulateError {
    StackUnderflow(TokenPath),
}

#[derive(Debug)]
struct TokenPath(usize, usize);

impl Display for TokenPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.0, self.1)
    }
}

#[derive(Debug)]
enum Token {
    Integer(TokenPath, i64),
    Plus(TokenPath),
    //  TODO: remove this token
    Print(TokenPath),
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
            self.column += 1;
            if buffer[0] == '\n' {
                self.line += 1;
                self.column = 1;
            }
            buffer = &buffer[1..];
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
            'a'..='z' | 'A'..='Z' => {
                let mut word = String::new();

                while !buffer.is_empty() && buffer[0].is_alphabetic() {
                    word.push(buffer[0]);
                    buffer = &buffer[1..];
                    self.column += 1;
                    if buffer[0] == '\n' {
                        self.line += 1;
                        self.column = 1;
                    }
                }

                self.buffer = buffer;

                match word.as_str() {
                    "print" => Some(Token::Print(token_path)),
                    _ => None,
                }
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
    fn compile(&mut self, writer: &mut impl std::io::Write) {
        writeln!(writer, ".segment text").unwrap();
        writeln!(writer, "global _start").unwrap();
        writeln!(writer, "_start:").unwrap();

        while let Some(token) = self.program.pop() {
            match token {
                Token::Integer(_, n) => {
                    writeln!(writer, "  ;; Push Integer").unwrap();
                    writeln!(writer, "  mov rax, {}", n).unwrap();
                    writeln!(writer, "  push rax").unwrap();
                }
                Token::Print(_) => todo!(),
                Token::Plus(_) => {
                    writeln!(writer, "  ;; Plus").unwrap();
                    writeln!(writer, "  pop rax").unwrap();
                    writeln!(writer, "  pop rbx").unwrap();
                    writeln!(writer, "  add rax, rbx").unwrap();
                    writeln!(writer, "  push rax").unwrap();
                }
            }
        }

        writeln!(writer, "  ;; Exit").unwrap();
        writeln!(writer, "  mov rax, 60").unwrap();
        writeln!(writer, "  mov rdi, 0").unwrap();
        writeln!(writer, "  syscall").unwrap();
    }
}

struct Simulator {
    program: Vec<Token>,
}

impl Simulator {
    fn run(&mut self) -> Result<(), KroksSimulateError> {
        dbg!(&self.program);

        let mut stack = Vec::new();

        while let Some(token) = self.program.pop() {
            match token {
                Token::Integer(_, n) => {
                    stack.push(n);
                }
                Token::Plus(path) => {
                    let (a, b) = match (stack.pop(), stack.pop()) {
                        (Some(a), Some(b)) => (a, b),
                        _ => return Err(KroksSimulateError::StackUnderflow(path)),
                    };
                    stack.push(a + b);
                }
                Token::Print(path) => {
                    let n = match stack.pop() {
                        Some(n) => n,
                        None => return Err(KroksSimulateError::StackUnderflow(path)),
                    };
                    println!("{}", n);
                }
            }
        }

        Ok(())
    }
}

fn usage(program: &str, mode: Option<KroksMode>) {
    match mode {
        Some(KroksMode::Compile) => {
            println!("Usage: {} com <file> <output>", program);
        }
        _ => {
            println!("Usage: {} [com | sim] <file>", program);
        }
    }
    exit(1);
}

fn main() {
    let args = std::env::args().collect::<Vec<String>>();

    if args.len() < 3 {
        usage(&args[0], None);
    }

    let buffer = std::fs::read_to_string(&args[2]).unwrap();
    let buffer = buffer.chars().collect::<Vec<char>>();

    let lexer = Lexer::new(&buffer);

    let stack: Vec<Token> = lexer.into_iter().collect();
    // TODO: remove this reverse
    let stack = stack.into_iter().rev().collect::<Vec<Token>>();

    match &args[1][..] {
        "com" => {
            let mut compiler = Compiler { program: stack };
            if args.len() < 4 {
                usage(&args[0], Some(KroksMode::Compile));
            }

            let mut output = std::fs::File::create(&args[3]).unwrap();
            let mut output = BufWriter::new(&mut output);

            compiler.compile(&mut output);
        }
        "sim" => {
            let mut simulator = Simulator { program: stack };

            if let Err(e) = simulator.run() {
                match e {
                    KroksSimulateError::StackUnderflow(path) => {
                        println!("Error: Stack underflow at {}", path);
                    }
                }
            }
        }
        _ => {
            usage(&args[0], None);
        }
    }
}
