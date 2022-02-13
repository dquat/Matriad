#![allow(warnings, unused)]
extern crate crossterm;

mod corul;

use std::borrow::BorrowMut;
use corul::message::Visualizer;
use corul::message::Msg;
use corul::util::Span;
use crate::corul::message::MsgType;

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
    println!("{:?}", get_ast());
}