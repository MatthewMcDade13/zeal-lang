#![feature(iter_intersperse)]

use std::fs;

use rustyline::{error::ReadlineError, DefaultEditor};

use crate::env::Env;

mod env;
mod interp;
mod lex;
mod val;

fn main() -> anyhow::Result<()> {
    // `()` can be used when no completer is required
    let mut rl = DefaultEditor::new()?;
    #[cfg(feature = "with-file-history")]
    if rl.load_history("repl-history.txt").is_err() {
        println!("No previous history.");
    }
    loop {
        let readline = rl.readline("wrath> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;
                let res = interp::rep(line, Env::default())?;
                println!("{}", res);
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    #[cfg(feature = "with-file-history")]
    rl.save_history("history.txt");
    Ok(())
}
