/*
 - compiler!
 - preprocessor
 - memory map
 - assembler?
*/


use std::{fs::OpenOptions, io::{Read, Write}};
use clap::Parser;

type Res<T> = Result<T, Box<dyn std::error::Error>>;

mod tokens;
mod ast;
mod codegen;

#[derive(Parser)]
struct Args {
    #[clap(short, long)]
    input: String,

    #[clap(short, long)]
    output: Option<String>,

    #[clap(long)]
    verbose_asm: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = {
        let args = Args::parse();
        if args.output.is_none() {
            Args {
                output: Some(args.input.replace(".c", ".asm")),
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
    let mut output = OpenOptions::new()
        .read(false)
        .write(true)
        .truncate(true)
        .create(true)
        .open(args.output.unwrap())?;

    let input = {
        let mut buf = String::with_capacity(input.metadata()?.len() as _);
        let mut input = std::io::BufReader::new(input);
        let count_read = input.read_to_string(&mut buf)?;
        println!("Read {count_read} bytes");
        buf
    };

    let program = tokens::parse(&input)?;
    let ctx = codegen::analyze(program, args.verbose_asm);
    let code = codegen::compile(ctx);

    output.write_all(code.as_bytes())?;

    Ok(())
}
