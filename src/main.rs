mod lexer;

use std::io::{prelude::Read, self};

use lexer::Token;
use pico_ml::Config;

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();

    let config = Config::new(&args).unwrap_or_else(|err| {
        eprintln!("Problem parsing arguments: {}", err);
        std::process::exit(1);
    });

    let mut s = String::new();
    if let "-" = config.filename.as_str() {
        if let Err(why) = std::io::stdin().read_to_string(&mut s) {
            eprintln!("Couldn't read stdin: {}", why);
            std::process::exit(1);
        }
    } else {
        let path = std::path::Path::new(&config.filename);
        let mut file = std::fs::File::open(&path).unwrap_or_else(|err| {
            eprintln!("Couldn't open {}: {}", path.display(), err);
            std::process::exit(1);
        });
        if let Err(why) = file.read_to_string(&mut s) {
            eprintln!("Couldn't read {}: {}", path.display(), why);
            std::process::exit(1);
        }
    };


    for t in lexer::Lexer::new(&s) {
        match t {
            Token::Unknown => panic!("Lex error"),
            Token::Whitespace => (),
            _ => println!("{:?}", t),
        }
    }
    Ok(())
}
