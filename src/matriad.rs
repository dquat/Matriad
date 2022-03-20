// modules
pub mod token;
pub mod message;
pub(crate) mod util;
pub mod lexer;

// imports
use std::thread;
use std::sync::mpsc::channel;
use std::time::{Duration, Instant};
use std::io::{Write, stdout};
use lexer::Lexer;

pub fn compile(source: String) {
    use crossterm::{
        ExecutableCommand,
        style::{Color, SetForegroundColor, ResetColor, Attribute, SetAttribute},
        cursor::{Hide, Show},
        terminal::{Clear, ClearType}
    };
    println!("Starting Compile:");
    let static_str: &'static str = Box::leak(source.into_boxed_str());
    let mut lexer = Lexer::new(static_str);
    let num = static_str.chars().count() as f64;
    let size = 30;
    let (itx, irx) = channel::<usize>();
    let (ttx, trx) = channel::<()>();
    let (dtx, drx) = channel::<()>();
    let lexer = thread::spawn(move || {
        while !lexer.eof() {
            if let Some(token) = lexer.next() {
                let _token = token;
                itx.send(lexer.pos);
            }
        }
        dtx.send(()).ok();
    });
    let progress = thread::spawn(move || {
        println!()
        // let time = Instant::now();
        // stdout().execute(Hide).unwrap();
        // loop {
        //     if drx.try_recv().is_ok() { break; }
        //     ttx.send(()).ok();
        //     let dist = irx.try_recv();
        //     if let Ok(dist) = dist {
        //         stdout().flush().unwrap();
        //         let percent = (dist as f64 / num) * 100.0;
        //         let curr_size = (percent * (size as f64 / 100.0)) as usize;
        //         stdout()
        //             .execute(Clear(ClearType::Purge)).unwrap();
        //         print!("\rlexing: {:.2}% [", percent);
        //         stdout()
        //             .execute(SetForegroundColor(Color::Blue)).unwrap()
        //             .execute(SetAttribute(Attribute::Bold)).unwrap();
        //         print!("{}>{}", "-".repeat(curr_size), " ".repeat(size - curr_size));
        //         stdout().execute(ResetColor).unwrap();
        //         print!("]");
        //         stdout().flush().unwrap();
        //     }
        //     thread::sleep(Duration::from_millis(100));
        // }
        // stdout().execute(Clear(ClearType::CurrentLine)).unwrap();
        // print!("\rlexing completed in {:?}.", time.elapsed());
        // stdout().execute(Show).unwrap();
    });
    lexer.join().unwrap();
    progress.join().unwrap();
    println!("\nDone compiling.");
}