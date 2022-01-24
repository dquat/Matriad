#![allow(warnings, unused)]
extern crate crossterm;

mod language;

use language::message::Visualizer;
use language::message::Msg;
use language::util::Span;
use crate::language::message::MsgType;

fn _black_box<T>(dummy: T) -> T {
    unsafe {
        let ret = std::ptr::read_volatile(&dummy);
        std::mem::forget(dummy);
        ret
    }
}

fn main() {
    let src = "\ntada bada\n\n\nbooom boom mooob\n soosunsuen nshhe dje jeiei";
    let rng = Span::from_range(11..22);
    // let mut exclude = Vec::new();
    vec![].collect::
    let mut vis = Visualizer::new_span(src, rng, "Main visualizer");
    dbg!(&vis);
    println!("{vis}");
    vis
        .compute_lines();
        // .compute_whitespace();
    // vis.wrap(15, 0, &mut exclude);
    dbg!(&vis);
    println!("{vis}");
    println!("{:?}", &src[rng.range()]);
    println!("{:?}", &vis.src[vis.span.range()]);
}