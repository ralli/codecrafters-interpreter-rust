use anyhow::Context;
use clap::{Parser, Subcommand};
use codecrafters_interpreter::{Ast, Interpreter, InterpreterError, Scanner, Value};
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::process::exit;
use std::rc::Rc;

#[derive(Parser)]
#[command(version, about, long_about = None)]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Tokenizes the input file
    Tokenize { filename: PathBuf },
    /// Parses the input file
    Parse { filename: PathBuf },
    /// Evaluates the input file
    Evaluate { filename: PathBuf },
    /// Runs the input file
    Run { filename: PathBuf },
}

fn main() -> Result<(), anyhow::Error> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Tokenize { filename } => {
            let file_contents = fs::read_to_string(&filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {:?}", filename);
                String::new()
            });
            let num_errors = tokenize(&file_contents)?;
            if num_errors > 0 {
                exit(65);
            }
        }
        Commands::Parse { filename } => {
            let file_contents = fs::read_to_string(&filename)
                .with_context(|| format!("cannot load file {:?}", &filename))?;
            let result = parse(&file_contents);
            match result {
                Ok(result) => println!("{result}"),
                Err(e) => {
                    eprintln!("{e}");
                    exit(65);
                }
            };
        }
        Commands::Evaluate { filename } => {
            let file_contents = fs::read_to_string(&filename)
                .with_context(|| format!("cannot load file {:?}", &filename))?;
            let result = evaluate(&file_contents);
            match result {
                Ok(result) => println!("{result}"),
                Err(e) => {
                    eprintln!("{e}");
                    exit(70);
                }
            };
        }
        Commands::Run { filename } => {
            let file_contents = fs::read_to_string(&filename)
                .with_context(|| format!("cannot load file {:?}", &filename))?;
            match run(&file_contents) {
                Err(InterpreterError::RuntimeError(e)) => {
                    eprintln!("{e}");
                    exit(70);
                }
                Err(InterpreterError::ParseError(e)) => {
                    eprintln!("{e}");
                    exit(65);
                }
                _ => (),
            }
        }
    }

    Ok(())
}

fn tokenize(input: &str) -> Result<usize, anyhow::Error> {
    let scanner = Scanner::new(input);
    let mut num_errors = 0;
    for token in scanner {
        match token {
            Ok(tok) => println!("{tok}"),
            Err(e) => {
                eprintln!("{e}");
                num_errors += 1
            }
        }
    }
    println!("EOF  null");
    Ok(num_errors)
}

fn parse(input: &str) -> Result<Rc<Ast>, anyhow::Error> {
    let mut parser = codecrafters_interpreter::Parser::new(input);
    parser.parse().map_err(Into::into)
}

fn evaluate(input: &str) -> Result<Value, anyhow::Error> {
    let mut parser = codecrafters_interpreter::Parser::new(input);
    let ast = parser.parse()?;
    let mut variables = HashMap::new();
    ast.eval(&mut variables).map_err(Into::into)
}

fn run(input: &str) -> Result<(), InterpreterError> {
    let mut interpreter = Interpreter::new(input);
    interpreter.run()
}
