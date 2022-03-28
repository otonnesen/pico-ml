mod parser;

mod lexer;

pub use parser::Parser;

pub struct Config {
    pub filename: String,
}

impl Config {
    /// Create a config.
    pub fn new(args: &[String]) -> Result<Config, &'static str> {
        if args.len() != 2 {
            return Err("usage: compiler <filename>");
        }

        let filename = args[1].clone();

        Ok(Config { filename })
    }
}
