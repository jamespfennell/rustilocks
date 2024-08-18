mod chunk;
mod compiler;
mod error;
mod scanner;
mod serde;
mod value;
mod vm;

use std::{path::PathBuf, process::exit};

use clap::{Parser, Subcommand};

/// Rustilocks: WIP implementation of the Lox bytecode intepreter in the book Crafting Interpreters.
#[derive(Parser, Debug)]
struct Cli {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Compile a Lox program to a Rustilocks .rlks bytecode file.
    Compile {
        /// File to compile.
        //
        /// This can be a Lox source file or Lox assembly file.
        input: PathBuf,
        /// Output path.
        ///
        /// If not set and compiling to bytecode, this will be the input path with a .rlks file extension.
        /// If not set and compiling to assembly, the output will be printed to stdout.
        output: Option<PathBuf>,
        /// Compile to a Lox assembly (.loxa) instead of bytecode.
        #[clap(long, action)]
        assembly: bool,
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
}

fn main() {
    let cli = Cli::parse();
    match cli.command {
        Command::Compile {
            input,
            output,
            assembly,
        } => {
            let chunk = match InputFile::read(&input) {
                InputFile::Lox(src) => compiler::compile(&src).unwrap(),
                InputFile::Assembly(src) => chunk::Chunk::deserialize_from_assembly(&src),
                InputFile::Binary(_) => panic!("can't compile .rlks files"),
            };
            if assembly {
                let out = chunk.serialize_to_assembly().unwrap();
                match output {
                    None => {
                        print!("{out}");
                    }
                    Some(output) => {
                        std::fs::write(output, out).unwrap();
                    }
                }
            } else {
                let output = match output {
                    None => {
                        let mut output = input.clone();
                        output.set_extension("rlks");
                        output
                    }
                    Some(output) => output,
                };
                std::fs::write(output, chunk.serialize()).unwrap();
            }
        }
        Command::Disassemble { input } => {
            let chunk = match InputFile::read(&input) {
                InputFile::Lox(_) => panic!("can't disassemble .lox files"),
                InputFile::Assembly(_) => panic!("can't disassemble .loxa files"),
                InputFile::Binary(src) => chunk::Chunk::deserialize(&src).unwrap(),
            };
            print!["{}", chunk.serialize_to_assembly().unwrap()];
        }
        Command::Run { input } => {
            let chunk = match InputFile::read(&input) {
                InputFile::Lox(src) => match compiler::compile(&src) {
                    Ok(chunk) => chunk,
                    Err(errors) => {
                        for error in errors {
                            eprintln!("{error}");
                        }
                        // TODO: should copy clox's return exit codes
                        exit(65);
                    }
                },
                InputFile::Assembly(s) => chunk::Chunk::deserialize_from_assembly(&s),
                InputFile::Binary(src) => chunk::Chunk::deserialize(&src).unwrap(),
            };
            let mut vm = vm::VM::default();
            if let Err(e) = vm.run(&chunk) {
                eprintln!("{e}\n[line {}] in script", e.line_number.unwrap_or(0));
                // TODO: should copy clox's return exit codes in all places
                exit(70);
            }
        }
        Command::Tokenize { input } => {
            let source_code = match InputFile::read(&input) {
                InputFile::Lox(c) => c,
                InputFile::Assembly(_) => panic!("can't tokenize .loxa files"),
                InputFile::Binary(_) => panic!("can't tokenize .rlks files"),
            };
            let mut scanner = scanner::Scanner::new(&source_code);
            while let Some(token) = scanner.next().unwrap() {
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
        match path.extension().and_then(|e| e.to_str()) {
            Some("lox") => InputFile::Lox(std::fs::read_to_string(path).unwrap()),
            Some("loxa") => InputFile::Assembly(std::fs::read_to_string(path).unwrap()),
            Some("rlks") => InputFile::Binary(std::fs::read(path).unwrap()),
            _ => {
                panic!("Only .lox, .loxa, and .rlks files are supported")
            }
        }
    }
}
