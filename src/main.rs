use clap::{Parser, Subcommand};
use codecrafters_interpreter::Scanner;
use std::fs;
use std::path::PathBuf;

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

            //  Uncomment this block to pass the first stage
            if !file_contents.is_empty() {
                let scanner = Scanner::new(&file_contents);
                for token in scanner {
                    let token = token?;
                    println!("{token}");
                }
                println!("EOF  null");
            } else {
                println!("EOF  null"); // Placeholder, remove this line when implementing the scanner
            }
        }
    }

    Ok(())
}
