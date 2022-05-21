// Unfinished & WIP file

// imports
use std::{
    borrow::{
        BorrowMut,
        Cow,
    },
    fmt::Display,
};

use crossterm::style::Stylize;

use crate::matriad::{
    token::{
        lexer::Set as InSet,
        lexthrow::Set as OutSet,
        Token,
    },
    message::*,
    util::Span,
    config::*,
};

type InToken  = Token<InSet>;
type OutToken = Token<OutSet>;

pub struct LexThrow<'a> {
    next_fn : &'a mut dyn FnMut() -> Option<InToken>,
    src     : &'a str,
}

impl<'a> LexThrow<'a> {
    pub fn new(next_fn: &'a mut impl FnMut() -> Option<InToken>, src: &'a str) -> Self {
        Self { next_fn, src }
    }

    pub fn next(&mut self) -> Option<OutToken> {
        let next = (self.next_fn)()?;
        match next.set {
            InSet::MultiLineComment { closed, depth, .. } => {
                if !closed {
                    // This feels much too long for generating a simple message, yeah?
                    let mut msg = self.msg("Unclosed multiline comment!", next.lines);
                    let vis = msg.add_vis();
                    let helper = &format!(
                        "Help: Add {} closing symbol{} to the end of your comment",
                        if depth > 1 { "these" } else { "this" },
                        if depth > 1 { "s" } else { "" },
                    );
                    let mut ptr =
                        Pointer::new('+', Span::new(next.pos.end, next.pos.end + depth * 3));
                    ptr.set_pointer_color(POINTER_PLUS_COLOR);
                    let val = &" */".repeat(depth);
                    ptr.set_insert(next.pos.end, val);
                    ptr.set_msg(helper, POINTER_HELP_MSG_COLOR);
                    vis.add_ptr(Pointer::new('^', Span::new(next.pos.start + 2, next.pos.end)));
                    vis.add_ptr(ptr);
                    let mut ptr =
                        Pointer::new('~', Span::new(next.pos.start, next.pos.start + 2));
                    ptr.set_pointer_color(POINTER_INFO_COLOR);
                    ptr.set_msg("This comment has not been closed", POINTER_HELP_MSG_COLOR);
                    vis.add_ptr(ptr);
                    msg.show();
                }
                self.next()
            },

            _ => {
                None
            }
        }
    }

    fn msg<S: Display + Clone>(&self, message: S, span: Span) -> Msg<S, &'a str> {
        Msg::new_span(message, "./file.mrd", self.src, span, MsgType::Error)
    }

    fn msg_type<S: Display + Clone>(
        &self,
        message  : S,
        span     : Span,
        msg_type : MsgType
    )
        -> Msg<S, &'a str>
    {
        Msg::new_span(message, "./file.mrd", self.src, span, msg_type)
    }
}