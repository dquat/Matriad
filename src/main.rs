// Created by dquat (https://github.com/dquat)

// temporarily disable warnings while developing
#![allow(warnings, unused)]

extern crate crossterm;

mod matriad;

// uses
use std::io::Write;
use std::time::{ Duration, Instant };
use crate::matriad::lexer::Lexer;
use crate::matriad::message::*;
use crate::matriad::util::*;

fn _point(points : &[(usize, &str)]) {
    let mut prev = 0;
    for i in 0..points.len() {
        // We have encountered the same position
        if points[i].0 + 1 == prev {
            // We're at the end and we have encountered the same position as before
            // Print this message too
            if i + 1 == points.len() { print!("> {}", points[i].1); }
            continue;
        }
        // Add required spaces
        for _ in 0..(points[i].0 - prev) { print!(" "); }
        // If we're at the end, print the message
        // If not, we print an extender (`|`)
        if i + 1 == points.len() { print!("{}", points[i].1); }
        else { print!("|"); }
        // Store the previously accessed location
        prev = points[i].0 + 1;
        std::io::stdout().flush().ok();
    }
}

fn point(points : &mut [(usize, &str)]) {
    points.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());
    for i in 0..points.len() {
        // Any padding to be applied to the left
        print!("");
        _point(&points[0..(points.len() - i)]);
        // Print a newline
        println!();
    }
}

// Test code stuff
fn main() {
    /// Uncomment this to check lex speed for a sample input at `matriad/test_source.mrd`
    /// Some of the syntax is just there for fun and this not what the language will actually
    /// end up looking like
    // lex_speed();

    // Stuff for the message class I'm playing with
    let mut points =
        [
            (1usize, "Ayo! Why did you do this, user?!"),
            (5, "This error! Just wow."),
            (11, "Did we just encounter 2 errors in one location?"),
            (11, "YES. Yes we did"),
            (14, "I'm last? Oh, yes I am. I'm the very last error!")
        ];
    println!("0123456789_123457890");
    println!(" ^   ^     ^  ^");
    point(&mut points);
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
    return avg;
}

/// Check the speed of the lexer
fn lex_speed() {
    // This test source gets lexed at approx. 270-280 MB/s on an i7-6500U CPU running Windows 10.
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