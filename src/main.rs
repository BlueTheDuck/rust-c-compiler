/*

void main() {}

*/

/*
 - compiler!
 - preprocessor
 - memory map
 - assembler?
*/

mod token;
mod ast;
mod compiler;

use std::{fs::OpenOptions, io::Read};

use clap::Parser;
use nom_locate::LocatedSpan;

#[derive(Parser)]
struct Args {
    #[clap(short, long)]
    input: String,

    #[clap(short, long)]
    output: Option<String>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = {
        let args = Args::parse();
        if args.output.is_none() {
            Args {
                output: Some(args.input.replace(".rs", ".asm")),
                ..args
            }
        } else {
            args
        }
    };
    
    let input = OpenOptions::new()
        .read(true)
        .write(false)
        .create(false)
        .open(args.input)?;
    let output = OpenOptions::new()
        .read(false)
        .write(true)
        .create(true)
        .open(args.output.unwrap())?;

    let input = {
        let mut buf = String::with_capacity(input.metadata()?.len() as _);
        let mut input = std::io::BufReader::new(input);
        let count_read = input.read_to_string(&mut buf)?;
        println!("Read {count_read} bytes");
        buf
    };

    let (rest, tokens) = token::tokenize(LocatedSpan::new(&input)).unwrap();
    println!("{:?}", tokens);


    Ok(())
}
