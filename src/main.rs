#![allow(warnings, unused)] // temporarily disable warnings for development
extern crate crossterm;

mod matriad;

use std::borrow::BorrowMut;
use std::time::{ Duration, Instant };
use matriad::message::Visualizer;
use matriad::message::Msg;
use matriad::util::Span;
use crate::matriad::message::MsgType;

// unused
fn _black_box<T>(dummy: T) -> T {
    unsafe {
        let ret = std::ptr::read_volatile(&dummy);
        std::mem::forget(dummy);
        ret
    }
}

// test code stuff
fn main() {
    let mut lex = matriad::lexer::Lexer::new(r#"test "strng" "\u{1234}" // comment
/* /* nested */ comment */ 192.32 23.21 1"#);
    while let Some(next) = lex.next() {
        println!("{next}");
    }

    let src = "\ntada bada\n\n\nbooom boom mooob\n soosunsuen nshhe dje jeiei";
    let rng = Span::from_range(11..22);
    println!("{:?}", &src[rng.range()]);
    let mut vis = Visualizer::new_span(src, rng, "Main visualizer");
    dbg!(&vis);
    println!("{vis}");
    vis
        .compute_lines();
        // .compute_whitespace();
    // vis.wrap(15, 0, &mut exclude);
    dbg!(&vis);
    println!("{vis}");
    println!("{:?}", &vis.range[vis.span.range()]);
}

fn bench(mut func: impl FnMut(), num: u32) {
    let time = Instant::now();
    for i in 0..num {
        func();
    }
    println!("Elapsed: {:?}", time.elapsed());
    println!("Single iteration: {:?}", time.elapsed() / num);
}