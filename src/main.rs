// Created by dquat (https://github.com/dquat)

// temporarily disable warnings while developing
#![allow(warnings, unused)]

extern crate crossterm;
extern crate unicode_width;

mod matriad;

// Just for tests
mod test;

// imports
use std::time::{
    Duration,
    Instant,
};
use crossterm::style::{Color, Stylize};
use crate::matriad::lexer::Lexer;
use crate::matriad::lexthrow::LexThrow;
use crate::matriad::message::*;
use crate::matriad::util::*;

// Test code stuff
fn main() {
    // Uncomment this to check lex speed for a sample input at `matriad/test_source.mrd`
    // Some of the syntax is just there for fun and this not what the language will actually
    // end up looking like
    // lex_speed();

    // Stuff for the message class I'm playing with

    // Try it out! you'll get a beautiful error!
    // Please don't look at how I created it. You might just loose your eyes!
    let src = "Hello.\n Welcome to this programmmmm /* Would you /* like */ \nto see an error?\n /* Sure thing! ";
    let mut lexer = Lexer::new(src);
    let mut next = || lexer.next();
    let mut lexerror = LexThrow::new(&mut next, src);
    for _ in 0..30 {
        lexerror.next();
    }
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
