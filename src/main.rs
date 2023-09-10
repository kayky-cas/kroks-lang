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

enum KroksCompileError {
    IO(std::io::Error),
    UnexpectedEof,
}

impl Display for KroksCompileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            KroksCompileError::UnexpectedEof => write!(f, "Unexpected end of file"),
            KroksCompileError::IO(err) => write!(f, "IO error: {}", err),
        }
    }
}

impl From<std::io::Error> for KroksCompileError {
    fn from(err: std::io::Error) -> Self {
        KroksCompileError::IO(err)
    }
}

enum KroksSimulateError {
    StackUnderflow(TokenPath),
    UnexpectedEof,
}

#[derive(Debug)]
struct TokenPath(usize, usize);

impl Display for TokenPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.0, self.1)
    }
}

#[derive(Debug)]
enum TokenKind {
    Integer(i64),
    Plus,
    Minus,
    //  TODO: remove this token
    Print64,
    Invalid,
}

#[derive(Debug)]
struct Token {
    path: TokenPath,
    kind: TokenKind,
}

impl Token {
    fn new(kind: TokenKind, path: TokenPath) -> Self {
        Token { path, kind }
    }
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
                }

                self.buffer = buffer;

                Some(Token::new(
                    TokenKind::Integer(number.parse::<i64>().unwrap()),
                    token_path,
                ))
            }
            'a'..='z' | 'A'..='Z' => {
                let mut word = String::new();

                while !buffer.is_empty() && buffer[0].is_alphanumeric() {
                    word.push(buffer[0]);
                    buffer = &buffer[1..];
                    self.column += 1;
                }

                self.buffer = buffer;

                match word.as_str() {
                    "print64" => Some(Token::new(TokenKind::Print64, token_path)),
                    _ => {
                        self.buffer = &buffer[1..];
                        Some(Token::new(TokenKind::Invalid, token_path))
                    }
                }
            }
            '+' => {
                self.buffer = &buffer[1..];
                Some(Token::new(TokenKind::Plus, token_path))
            }
            '-' => {
                self.buffer = &buffer[1..];
                Some(Token::new(TokenKind::Minus, token_path))
            }
            _ => {
                self.buffer = &buffer[1..];
                Some(Token::new(TokenKind::Invalid, token_path))
            }
        }
    }
}

struct Compiler {
    program: Vec<Token>,
}

impl Compiler {
    fn compile(&mut self, writer: &mut impl std::io::Write) -> Result<(), KroksCompileError> {
        writeln!(writer, "segment .text")?;
        writeln!(writer, "global _start")?;
        writeln!(writer, "_start:")?;

        while let Some(token) = self.program.pop() {
            match token.kind {
                TokenKind::Integer(n) => {
                    twriteln!(writer, ";; Push Integer")?;
                    twriteln!(writer, "mov rax, {}", n)?;
                    twriteln!(writer, "push rax")?;
                }
                TokenKind::Plus => {
                    twriteln!(writer, ";; Plus")?;
                    twriteln!(writer, "pop rax")?;
                    twriteln!(writer, "pop rbx")?;
                    twriteln!(writer, "add rax, rbx")?;
                    twriteln!(writer, "push rax")?;
                }
                TokenKind::Minus => {
                    twriteln!(writer, ";; Minus")?;
                    twriteln!(writer, "pop rax")?;
                    twriteln!(writer, "pop rbx")?;
                    twriteln!(writer, "sub rax, rbx")?;
                    twriteln!(writer, "push rax")?;
                }
                TokenKind::Print64 => {
                    twriteln!(writer, ";; Print")?;
                    twriteln!(writer, "pop rdi")?;
                    twriteln!(writer, "call print64")?;
                }
                TokenKind::Invalid => Err(KroksCompileError::UnexpectedEof)?,
            }
        }

        twriteln!(writer, ";; Exit")?;
        twriteln!(writer, "mov rax, 60")?;
        twriteln!(writer, "mov rdi, 0")?;
        twriteln!(writer, "syscall")?;

        // TODO: remove this when print is implemented
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
        let mut stack = Vec::new();

        while let Some(token) = self.program.pop() {
            match token.kind {
                TokenKind::Integer(n) => {
                    stack.push(n);
                }
                TokenKind::Plus => {
                    let (a, b) = match (stack.pop(), stack.pop()) {
                        (Some(a), Some(b)) => (a, b),
                        _ => return Err(KroksSimulateError::StackUnderflow(token.path)),
                    };
                    stack.push(a + b);
                }
                TokenKind::Minus => {
                    let (a, b) = match (stack.pop(), stack.pop()) {
                        (Some(a), Some(b)) => (a, b),
                        _ => return Err(KroksSimulateError::StackUnderflow(token.path)),
                    };
                    stack.push(a - b);
                }
                TokenKind::Print64 => {
                    let n = match stack.pop() {
                        Some(n) => n,
                        None => return Err(KroksSimulateError::StackUnderflow(token.path)),
                    };
                    println!("{}", n);
                }
                TokenKind::Invalid => return Err(KroksSimulateError::UnexpectedEof),
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

    let mut stack: Vec<Token> = Vec::new();

    for token in lexer.into_iter() {
        match token.kind {
            TokenKind::Invalid => {
                println!("Error: Invalid token at {}", token.path);
                process::exit(1);
            }
            _ => stack.push(token),
        }
    }

    stack.reverse();

    match &args[1][..] {
        "com" => {
            let mut compiler = Compiler { program: stack };
            if args.len() < 4 {
                usage(&args[0], Some(KroksMode::Compile));
            }

            let asm_filename = format!("{}.asm", args[3]);
            let mut output = std::fs::File::create(&asm_filename).unwrap();
            let mut output = BufWriter::new(&mut output);

            if let Err(e) = compiler.compile(&mut output) {
                match e {
                    KroksCompileError::UnexpectedEof => {
                        eprintln!("Error: Unexpected end of file");
                    }
                    KroksCompileError::IO(err) => {
                        eprintln!("Error: {}", err);
                    }
                }
                process::exit(1);
            }

            let mut nasm = Command::new("nasm");
            nasm.arg("-felf64");
            nasm.arg("-o");
            nasm.arg(format!("{}.o", args[3]));
            nasm.arg(asm_filename);
            let nasm = nasm.output().unwrap();

            if !nasm.status.success() {
                println!("Error: {}", String::from_utf8_lossy(&nasm.stderr));
                process::exit(1);
            }

            let mut ld = Command::new("ld");
            ld.arg("-o");
            ld.arg(&args[3]);
            ld.arg(format!("{}.o", args[3]));
            let ld = ld.output().unwrap();

            if !ld.status.success() {
                println!("Error: {}", String::from_utf8_lossy(&ld.stderr));
                process::exit(1);
            }
        }
        "sim" => {
            let mut simulator = Simulator { program: stack };

            if let Err(e) = simulator.run() {
                match e {
                    KroksSimulateError::StackUnderflow(path) => {
                        eprintln!("Error: Stack underflow at {}", path);
                    }
                    KroksSimulateError::UnexpectedEof => {
                        eprintln!("Error: Unexpected end of file");
                    }
                }
            }
        }
        _ => {
            usage(&args[0], None);
        }
    }
}
