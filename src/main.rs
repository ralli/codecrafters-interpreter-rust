use clap::{Parser, Subcommand};
use codecrafters_interpreter::Scanner;
use std::fs;
use std::path::PathBuf;
use std::process::exit;

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
}

fn main() -> Result<(), anyhow::Error> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Tokenize { filename } => {
            let file_contents = fs::read_to_string(&filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {:?}", filename);
                String::new()
            });

            let scanner = Scanner::new(&file_contents);
            let mut num_errors = 0;
            for token in scanner {
                match token {
                    Ok(tok) => println!("{tok}"),
                    Err(e) => {
                        eprintln!("{e}");
                        num_errors += 1
                    },
                }
            }
            println!("EOF  null");
            if num_errors > 0 {
                exit(65);
            }
        }
    }

    Ok(())
}
