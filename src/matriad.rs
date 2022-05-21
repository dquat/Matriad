// modules
pub mod lexthrow;
pub mod message;
pub mod token;
pub mod lexer;
pub mod util;
pub(self) mod config;

// imports
use std::{
    thread,
    sync::mpsc::channel,
    time::Instant,
    io::{
        Write,
        stdout
    }
};

use lexer::Lexer;

fn progress<R: Copy + Send + 'static>(
    mut function    : impl FnMut() -> Option<R> + Send + 'static,
    send_check      : impl Fn(R) -> bool + Send + 'static,
    calc_percentage : impl Fn(R) -> f64 + Send + 'static,
    name            : &'static str,
    size            : usize,
) {
    use crossterm::{
        ExecutableCommand,
        style::{
            Color,
            SetForegroundColor,
            ResetColor,
            Attribute,
            SetAttribute
        },
        cursor::{
            Hide,
            Show
        },
        terminal::{
            Clear,
            ClearType
        }
    };
    let (itx, irx) = channel::<R>();
    let (dtx, drx) = channel::<()>();
    let process = thread::spawn(move || {
        while let Some(res) = function() {
            if send_check(res) {
                itx.send(res).ok();
            }
        }
        dtx.send(()).ok();
    });
    let progress = thread::spawn(move || {
        let time = Instant::now();
        stdout().execute(Hide).unwrap();
        loop {
            if drx.try_recv().is_ok() { break; }
            stdout().flush().unwrap();
            if let Ok(val) = irx.try_recv() {
                let percent    = calc_percentage(val);
                let curr_size = (percent * (size as f64 / 100.0)).ceil() as usize;
                stdout().execute(Clear(ClearType::Purge)).unwrap();
                print!("\r{name}... {percent:.2}% [");
                stdout()
                    .execute(SetForegroundColor(Color::Blue)).unwrap()
                    .execute(SetAttribute(Attribute::Bold)).unwrap();
                print!("{}>{}", "=".repeat(curr_size), " ".repeat(size - curr_size));
                stdout().execute(ResetColor).unwrap();
                print!("]");
                stdout().flush().unwrap();
            }
        }
        stdout().execute(Clear(ClearType::CurrentLine)).unwrap();
        print!("\r{name} completed in {:?}.", time.elapsed());
        stdout().execute(Show).unwrap();
    });
    process.join().unwrap();
    progress.join().unwrap();
}

pub fn compile(source: String) {
    println!("Starting Compile:");
    let static_str: &'static str = Box::leak(source.into_boxed_str());
    let mut lexer = Lexer::new(static_str);
    let num = static_str.chars().count() as f64;
    let size = 30;
    let interval = num as usize / size;
    // progress bar makes lexing ~20-25% slower
    progress(
        move || {
            if let Some(token) = lexer.next() {
                let _token = token;
                Some(lexer.pos)
            } else {
                None
            }
        },
        move |pos| pos % interval < size,
        move |val| (val as f64 / num) * 100.0,
        "lexing",
        size
    );
    println!("\nDone compiling.");
}