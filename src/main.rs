use std::io;
use std::io::Read;

use pico_ml::{pprint, typecheck, Config, Parser};

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();

    let config = Config::new(&args).unwrap_or_else(|err| {
        eprintln!("Problem parsing arguments: {}", err);
        std::process::exit(1);
    });

    let s = get_input(config);

    let program = Parser::new(&s).program().unwrap_or_else(|err| {
        eprintln!("Couldn't parse input: {}", err);
        std::process::exit(1);
    });

    pprint(&program, 2);

    let prog_type = typecheck(&program).unwrap_or_else(|err| {
        eprintln!("Error typechecking: {}", err);
        std::process::exit(1);
    });

    println!("{:?}", prog_type);

    Ok(())
}

fn get_input(config: Config) -> String {
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
    s
}
