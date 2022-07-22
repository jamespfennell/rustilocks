mod chunk;
mod error;
mod scanner;
mod value;
mod vm;

use std::path::PathBuf;

use clap::{Parser, Subcommand};

/// Rustilocks: WIP implementation of the Lox bytecode intepreter in the book Crafting Interpreters.
#[derive(Parser, Debug)]
struct Cli {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Compile a Lox program to a Rustilocks .rlks binary file.
    Compile {
        /// File to compile.
        //
        /// This can be a Lox source file or Lox assembly file.
        input: PathBuf,
        /// Output path. If not set, this will be the input path with a .rlks file extension.
        output: Option<PathBuf>,
    },
    /// Disassemble a Rustilocks .rlks binary file.
    Disassemble {
        /// File to disassemble.
        input: PathBuf,
    },
    /// Run a Lox program.
    Run {
        /// File to run.
        ///
        /// This can be a Lox source file, Lox assembly file, or Rustilocks .rlks binary file.
        input: PathBuf,
    },
    /// Parse a Lox source file and print the resulting tokens.
    Tokenize {
        /// Lox source file to parse.
        input: PathBuf,
    },
    // Assemble, Disassemble
}

fn main() {
    let cli = Cli::parse();
    match cli.command {
        Command::Compile { input, output } => {
            let source_code = match InputFile::read(&input) {
                InputFile::Lox(_) => panic!("can't compile .lox files (yet)"),
                InputFile::Assembly(s) => s,
                InputFile::Binary(_) => panic!("can't compile .rlks files"),
            };
            let chunk = chunk::Chunk::assemble(&source_code);
            let output = match output {
                None => {
                    let mut output = input.clone();
                    output.set_extension("rlks");
                    output
                }
                Some(output) => output,
            };
            std::fs::write(&output, chunk.serialize().unwrap()).unwrap();
        }
        Command::Disassemble { input } => {
            let chunk = match InputFile::read(&input) {
                InputFile::Lox(_) => panic!("can't disassemble .lox files"),
                InputFile::Assembly(_) => panic!("can't disassemble .lox files"),
                InputFile::Binary(src) => chunk::Chunk::deserialize(&src),
            };
            chunk.disassemble().unwrap();
        },
        Command::Run { input } => {
            let chunk = match InputFile::read(&input) {
                InputFile::Lox(_) => panic!("can't run .lox files (yet)"),
                InputFile::Assembly(s) => chunk::Chunk::assemble(&s),
                InputFile::Binary(src) => chunk::Chunk::deserialize(&src),
            };
            vm::run(&chunk).unwrap();
        }
        Command::Tokenize { input } => {
            let source_code = match InputFile::read(&input) {
                InputFile::Lox(c) => c,
                InputFile::Assembly(_) => panic!("can't tokenize .loxa files"),
                InputFile::Binary(_) => panic!("can't tokenize .rlks files"),
            };
            let mut scanner = scanner::Scanner {
                source: &source_code,
            };
            while let Some(token) = scanner.scan().unwrap() {
                println!("{:?}", token)
            }
        }
    }
}

enum InputFile {
    Lox(String),
    Assembly(String),
    Binary(Vec<u8>),
}

impl InputFile {
    fn read(path: &PathBuf) -> InputFile {
        match path.extension().map_or(None, |e| e.to_str()) {
            Some("lox") => InputFile::Lox(std::fs::read_to_string(path).unwrap()),
            Some("loxa") => InputFile::Assembly(std::fs::read_to_string(path).unwrap()),
            Some("rlks") => InputFile::Binary(std::fs::read(path).unwrap()),
            _ => {
                panic!("Only .lox, .loxa, and .rlks files are supported")
            }
        }
    }
}
