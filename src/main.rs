// Created by dquat (https://github.com/dquat)

// temporarily disable warnings while developing
#![allow(warnings, unused)]

extern crate crossterm;
extern crate unicode_width;

mod matriad;

// Just for tests
#[cfg(test)]
mod test;

use std::path::Path;
// imports
use std::time::{
    Duration,
    Instant,
};
use crate::matriad::lexer::Lexer;
use crate::matriad::lexthrow::LexThrow;

// Test code stuff
fn main() {
    // Uncomment this to check lex speed for a sample input at `matriad/test_source.mrd`
    // Some of the syntax is just there for fun and this not what the language will actually
    // end up looking like
    // lex_speed();

    // Lex a given program!
    let mut args = std::env::args();
    let program = args.next().unwrap_or(String::from(" matriad"));
    let last_idx = match program.rfind("/") {
        Some(v) => v,
        None => match program.rfind("\\") {
            Some(v) => v,
            None => 0,
        },
    };
    let program = &program[last_idx + 1..];
    if let Some(mut file) = args.next() {
        if !file.ends_with(".mrd") {
            file.push_str(".mrd");
        }
        println!("Trying to read file `{}`.", file);
        let path = Path::new(&file);
        match std::fs::read_to_string(path) {
            Ok(src) => {
                println!("File read successfully!");
                println!("Trying to lex contents of file...");
                let src =
                    src
                        // Remove carriage returns since the message class uses newlines to display
                        // messages, and carriage returns throw it's locations all over the place
                        .replace('\r', "")
                        // Tabs will be replaced with 4 spaces just for convenience
                        // Mainly so that I don't have to do extra work in generating a message
                        .replace('\t', "    ");
                let elapsed = Instant::now();
                let mut lexer = Lexer::new(&src);
                let mut next = || lexer.next();
                let mut lexerror = LexThrow::new(&mut next, &src);
                let mut error_count = 0;
                loop {
                    match lexerror.next() {
                        // A value was generated, print it out
                        Ok(val) => {
                            println!("{val:#?}")
                        },
                        // We found an EOF
                        Err(crate::matriad::lexthrow::LexError::EOF) => break,
                        // An error was generated, do nothing
                        _ => error_count += 1,
                    };
                }
                let elapsed = elapsed.elapsed();
                if error_count > 0 {
                    println!("Encountered {error_count} error(s).");
                }
                println!("Finished lexing and printing in {elapsed:?}.");
            },

            Err(error) => println!("Failed to read file `{}` because of the error: {}", file, error.kind()),
        };
    } else {
        println!("Please enter a file name to lex.");
        println!("Run `{program} <file> | <file>.mrd | </path/file> | </path/file>.mrd` to lex a specific file.");
    }

    // // Try it out! you'll get a beautiful error!
    // // Please don't look at how I created it. You might just loose your eyes!
    // // let src = "Hello👪.\n Welcome to this programm👪mmm /* Would 👪you /* like */-\nto see an 👪error?\n /* Suore t👪hing!-";
    // // let src = "bb👪\naa \"wh👪 wah!\n\t ayy\\ y👪\nhelo";
    // let src = "\"bee \n\\\n         lleee\"";
    // // Tabs will be replaced with 4 spaces just for convenience
    // // Mainly so that I don't have to do extra work in generating a message
    // let src = src.replace('\t', "    ");
    // let mut lexer = Lexer::new(&src);
    // let mut next = || lexer.next();
    // let mut lexerror = LexThrow::new(&mut next, &src);
    // loop {
    //     match lexerror.next() {
    //         // A value was generated
    //         Ok(val) => {
    //             println!("{val:#?}")
    //         },
    //         // we found an EOF
    //         Err(crate::matriad::lexthrow::LexError::EOF) => break,
    //         _ => ()
    //     };
    // }
}

fn bench(mut func: impl FnMut(), num: u32, samples: u32) -> Duration {
    let mut max = Duration::from_secs(0);
    let mut min = Duration::from_secs(u64::MAX);
    let mut tot = Duration::from_secs(0);
    for _ in 0..samples {
        let time = Instant::now();
        for _ in 0..num { func(); }
        let elapsed = time.elapsed();
        if elapsed > max { max = elapsed; }
        if elapsed < min { min = elapsed; }
        tot += elapsed;
    }
    println!("Total elapsed: {:?}", tot);
    let avg = (tot / 100) / num;
    println!("[max: {:?}, min: {:?}, avg: {:?}]", max / num, min / num, avg);
    avg
}

/// Check the speed of the lexer
fn lex_speed() {
    // This test source gets lexed at approx. 285-295 MB/s on an i7-6500U CPU running Windows 10.
    // Each source will get lexed at different speeds as not all sources contain the same number
    // of comments, numbers, strings etc.
    // But this should give you a general idea about it's average speed
    let file = "./test_source.mrd";
    let contents = std::fs::read_to_string(file);
    if let Ok(src) = contents {
        let src = src.as_str();
        println!("Source length: {}", src.len());
        // This is the benchmark function
        // If you find it a non "valid" bench, you can change it
        let bench_fn = || {
            let mut lexer = Lexer::new(src);
            // This loop does not get optimized out
            while let Some(_) = lexer.next() {
                /* Benchmarked! */
            }
        };
        // This runs the bench 5000 * 100 times, i.e. 500k times
        let avg = bench(bench_fn, 5_000, 100);
        // 1ns = 1 billionth of a second
        let bytes_per_sec = (1_000_000_000.0 / avg.as_nanos() as f64) * src.len() as f64;
        // 1000  B = 1KB, 1024  B = 1KB
        // 1000 KB = 1MB, 1024 KB = 1MiB
        let mb  = 1000.0 * 1000.0;
        let mib = 1024.0 * 1024.0;
        println!("Average lex speed: {:.3} MB/s => {:.3} MiB/s", bytes_per_sec / mb, bytes_per_sec / mib);
    } else {
        println!("Failed to read source file: `{file}`!");
    }
}
