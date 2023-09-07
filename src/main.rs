use std::{
    fmt::{self, Display, Formatter},
    io::BufWriter,
    process::{self, exit, Command},
};

// TODO: create assembly macro for writing
macro_rules! twriteln {
    ($writer:expr, $($arg:tt)*) => {
        writeln!($writer, "\t{}", format!($($arg)*))
    };
}

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
    fn compile(&mut self, writer: &mut impl std::io::Write) -> Result<(), std::io::Error> {
        writeln!(writer, "segment .text")?;
        writeln!(writer, "global _start")?;
        writeln!(writer, "_start:")?;

        while let Some(token) = self.program.pop() {
            match token {
                Token::Integer(_, n) => {
                    twriteln!(writer, ";; Push Integer")?;
                    twriteln!(writer, "mov rax, {}", n)?;
                    twriteln!(writer, "push rax")?;
                }
                Token::Plus(_) => {
                    twriteln!(writer, ";; Plus")?;
                    twriteln!(writer, "pop rax")?;
                    twriteln!(writer, "pop rbx")?;
                    twriteln!(writer, "add rax, rbx")?;
                    twriteln!(writer, "push rax")?;
                }
                Token::Print(_) => {
                    twriteln!(writer, ";; Print")?;
                    twriteln!(writer, "pop rdi")?;
                    twriteln!(writer, "call print64")?;
                }
            }
        }

        // TODO: remove this when print is implemented

        twriteln!(writer, ";; Exit")?;
        twriteln!(writer, "mov rax, 60")?;
        twriteln!(writer, "mov rdi, 0")?;
        twriteln!(writer, "syscall")?;

        writeln!(writer, "print64:")?;
        twriteln!(writer, "sub     rsp, 40")?;
        twriteln!(writer, "mov     BYTE [rsp+31], 10")?;
        twriteln!(writer, "test    rdi, rdi")?;
        twriteln!(writer, "je      .L4")?;
        twriteln!(writer, "mov     rsi, 7378697629483820647")?;
        twriteln!(writer, "mov     ecx, 29")?;

        writeln!(writer, ".L3:")?;
        twriteln!(writer, "mov     rax, rdi")?;
        twriteln!(writer, "imul    rsi")?;
        twriteln!(writer, "mov     rax, rdi")?;
        twriteln!(writer, "sar     rax, 63")?;
        twriteln!(writer, "sar     rdx, 2")?;
        twriteln!(writer, "sub     rdx, rax")?;
        twriteln!(writer, "lea     rax, [rdx+rdx*4]")?;
        twriteln!(writer, "add     rax, rax")?;
        twriteln!(writer, "sub     rdi, rax")?;
        twriteln!(writer, "mov     rax, rcx")?;
        twriteln!(writer, "add     edi, 48")?;
        twriteln!(writer, "mov     BYTE [rsp+1+rcx], dil")?;
        twriteln!(writer, "mov     rdi, rdx")?;
        twriteln!(writer, "sub     rcx, 1")?;
        twriteln!(writer, "test    rdx, rdx")?;
        twriteln!(writer, "jne     .L3")?;
        twriteln!(writer, "cdqe")?;
        twriteln!(writer, "mov     edx, 32")?;
        twriteln!(writer, "sub     rdx, rax")?;
        writeln!(writer, ".L2:")?;
        twriteln!(writer, "lea     rsi, [rsp+rax]")?;
        twriteln!(writer, "mov     edi, 1")?;
        twriteln!(writer, "mov     eax, 1")?;
        twriteln!(writer, "syscall")?;
        twriteln!(writer, "add     rsp, 40")?;
        twriteln!(writer, "ret")?;
        writeln!(writer, ".L4:")?;
        twriteln!(writer, "mov     edx, 2")?;
        twriteln!(writer, "mov     eax, 30")?;
        twriteln!(writer, "jmp     .L2")?;

        writer.flush()?;

        Ok(())
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

    let buffer = match std::fs::read_to_string(&args[2]) {
        Ok(buffer) => buffer,
        Err(err) => {
            println!("Error: {}", err);
            process::exit(1);
        }
    };

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

            let asm_filename = format!("{}.asm", args[3]);
            let mut output = std::fs::File::create(&asm_filename).unwrap();
            let mut output = BufWriter::new(&mut output);

            match compiler.compile(&mut output) {
                Ok(_) => {
                    Command::new("nasm")
                        .arg("-f")
                        .arg("elf64")
                        .arg("-ggdb")
                        .arg("-o")
                        .arg(&format!("{}.o", args[3]))
                        .arg(&asm_filename)
                        .output()
                        .expect("failed to execute process");

                    Command::new("ld")
                        .arg("-o")
                        .arg(&args[3])
                        .arg(&format!("{}.o", args[3]))
                        .output()
                        .expect("failed to execute process");
                }
                Err(err) => {
                    println!("Error: {}", err);
                    process::exit(1);
                }
            }
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
