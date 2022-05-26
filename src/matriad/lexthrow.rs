// Unfinished & WIP file

// imports
use std::{
    borrow::Cow,
    fmt::Display,
};

use crate::matriad::{
    token::{
        lexer::Set as InSet,
        lexthrow::{
            Set as OutSet,
            NumberKind,
            NumberPrecision,
            StringSet,
            StringValue,
        },
        Token,
        NumberSet,
        StrSet,
        CharSet,
        RawStrErr,
    },
    message::*,
    util::Span,
    config::*,
};

pub enum LexError {
    /// End of file
    EOF,
    /// An error was found
    LexError,
}

type InToken  = Token<InSet>;
type OutToken = Token<OutSet>;

pub struct LexThrow<'a> {
    next_fn : &'a mut dyn FnMut() -> Option<InToken>,
    start   : usize,
    end     : usize,
    src     : &'a str,
}

impl<'a> LexThrow<'a> {
    pub fn new(next_fn: &'a mut impl FnMut() -> Option<InToken>, src: &'a str) -> Self {
        Self { next_fn, src, start: 0, end: 0 }
    }

    pub fn next(&mut self) -> Result<OutToken, LexError> {
        let next = match (self.next_fn)() {
            Some(v) => v,
            None => return Err(LexError::EOF),
        };
        self.start = next.pos.start;
        self.end = next.pos.end;
        match next.set {

            InSet::Whitespace => self.next(),

            InSet::Delimiter(delim) => Ok(Token::new(OutSet::Delimiter(delim), next.pos, next.lines)),

            InSet::Identifier => Ok(Token::new(OutSet::Identifier, next.pos, next.lines)),

            InSet::SingleLineComment { .. } => self.next(),

            // Check integers
            InSet::Int { set, empty, suffix_start } => {
                let mut error = false;
                // If the empty flag is true, it means that the set is
                // octal, binary or hex and has no extension!
                if empty {
                    let message = format!(
                            "Expected at least a single digit after the start of a {} integer!",
                            set
                                .to_string()
                                .to_lowercase()
                        );
                    self
                        .msg(message, next.lines)
                        .set_code_("E0005")
                        .add_vis_()
                        .add_ptr('^', next.pos)
                        .add_ptr('+', Span::single(self.end))
                        .set_ptr_color(POINTER_PLUS_COLOR)
                        .set_ptr_insert(self.end, "0001")
                        .set_ptr_msg("Help: Add an extension here, ex: `0001`", POINTER_HELP_MSG_COLOR)
                        .show();
                    error = true;
                }
                // parse suffix
                let suffix = &self.src[suffix_start..self.end];
                let mut res = None;
                match Self::is_int_suffix(suffix) {
                    Some((precision, is_uint)) => {
                        res = Some(OutSet::Number {
                            set,
                            precision,
                            kind     : if is_uint { NumberKind::UnsignedInteger } else { NumberKind::Integer },
                            exponent : 1,
                        });
                    },
                    None => {
                        // Well, the suffix is invalid, so generate an error
                        let message = format!(
                                "Expected one of `i8`, `i16`, `i32`, `i64`, `isize`, `u128` etc. as a suffix, but found the suffix `{}`!",
                                suffix
                            );
                        self
                            .msg(message, next.lines)
                            .set_code_("E0006")
                            .add_vis_()
                            // Information
                            .add_ptr('-', Span::new(self.start, suffix_start))
                            .set_ptr_color(POINTER_INFO_COLOR)
                            .set_ptr_msg("Info: This integer has an invalid suffix", POINTER_INFO_MSG_COLOR)
                            // Error
                            .add_ptr('^', Span::new(suffix_start, self.end))
                            .set_ptr_msg("Help: Try replacing this with one of the above suffixes", POINTER_HELP_MSG_COLOR)
                            .show();
                        error = true;
                    }
                };
                if error {
                    Err(LexError::LexError)
                } else {
                    Ok(OutToken::new(res.unwrap(), Span::new(self.start, suffix_start), next.lines))
                }
            }

            // Check floats
            InSet::Float { set, empty, number, exponent, suffix_start } => {
                let mut error = false;
                // We cannot have floating point octal, binary or hexadecimal numbers, can we now?
                if set != NumberSet::Normal {
                    self
                        .msg(format!("{} numbers cannot have a floating point value!", set), next.lines)
                        .set_code_("E0002")
                        .add_vis_()
                        .add_ptr('^', next.pos)
                        .show();
                    error = true;
                }
                // If the empty flag is true, it means the exponent has no digit!
                if empty {
                    self
                        .msg("Expected at least a single digit after an exponent!", next.lines)
                        .set_code_("E0003")
                        .add_vis_()
                        // Error
                        .add_ptr('^', next.pos)
                        // Help
                        .add_ptr('+', Span::single(self.end))
                        .set_ptr_color(POINTER_PLUS_COLOR)
                        .set_ptr_insert(self.end, "15")
                        .set_ptr_msg("Help: Add a number here, ex: `15`", POINTER_HELP_MSG_COLOR)
                        .show();
                    error = true;
                }
                // parse exponent
                let exponent_str = &self.src[exponent.start..exponent.end];
                let mut exponent = 1;
                if !exponent_str.is_empty() {
                    exponent =
                        exponent_str
                            .parse()
                            // This should never panic since it's checked to be a number by the lexer
                            .unwrap();
                }
                // parse suffix
                let suffix = &self.src[suffix_start..self.end];
                let mut res = None;
                match Self::is_float_suffix(suffix) {
                    Some(precision) => {
                        res = Some(OutSet::Number {
                            set,
                            exponent,
                            precision,
                            kind     : NumberKind::Float,
                        });
                    },
                    None => {
                        let message = format!(
                            "Expected one of `f32` or `f64` as a suffix, but found the suffix `{}`!",
                            suffix
                        );
                        // Well, the suffix is invalid, so generate an error
                        self
                            .msg(message, next.lines)
                            .set_code_("E0004")
                            .add_vis_()
                            // Information
                            .add_ptr('-', Span::new(self.start, suffix_start))
                            .set_ptr_color(POINTER_INFO_COLOR)
                            .set_ptr_msg("Info: This float has an invalid suffix", POINTER_INFO_MSG_COLOR)
                            // Help and error
                            .add_ptr('^', Span::new(suffix_start, self.end))
                            .set_ptr_msg("Help: Try replacing this with `f32` or `f64`", POINTER_HELP_MSG_COLOR)
                            .show();
                        error = true;
                    }
                };
                if error {
                    Err(LexError::LexError)
                } else {
                    Ok(OutToken::new(res.unwrap(), number, next.lines))
                }
            }

            // Check multiline comments
            InSet::MultiLineComment { closed, depth, .. } => {
                if !closed {
                    let helper = &format!(
                        "Help: Add th{} closing symbol{} to the end of your comment",
                        if depth > 1 { "ese" } else { "is" },
                        if depth > 1 { "s"   } else { ""   },
                    );
                    let val = &" */".repeat(depth);

                    // This looks way better now. Almost... beautiful
                    self
                        .msg("Unclosed multiline comment!", next.lines)
                        .set_code_("E0001")
                        .add_vis_()
                        // Information
                        .add_ptr('-', Span::new(self.start, self.start + 2))
                        .set_ptr_color(POINTER_INFO_COLOR)
                        .set_ptr_msg("Info: This comment has not been closed", POINTER_INFO_MSG_COLOR)
                        // Show error
                        .add_ptr('^', Span::new(self.start + 2, self.end))
                        // Helper
                        .add_ptr('+', Span::single(self.end))
                        .set_ptr_color(POINTER_PLUS_COLOR)
                        .set_ptr_insert(self.end, val)
                        .set_ptr_msg(helper, POINTER_HELP_MSG_COLOR)
                        .show();
                }
                self.next()
            },

            // Check if the string is valid and return it
            InSet::String { set } => {
                match set {
                    StrSet::Normal { closed } => {
                        if !closed {
                            self.unclosed(false, &next);
                        }
                        let span = Span::new(self.start + 1, self.end - 1);
                        match self.parse_string(&self.src[span.range()], next.lines, 1, false) {
                            Some(string) => {
                                let val = string;
                                Ok(Token::new(
                                    OutSet::String {
                                        value : StringValue::String(val),
                                        set   : StringSet::Normal
                                    },
                                    next.pos,
                                    next.lines
                                ))
                            },
                            None => {
                               Ok(Token::new(
                                   OutSet::String {
                                       value : StringValue::Span(span),
                                       set   : StringSet::Normal
                                   },
                                   next.pos,
                                   next.lines
                               ))
                            }
                        }
                    },

                    StrSet::ByteStr { closed } => {
                        if !closed {
                            self.unclosed(false, &next);
                        }
                        let span = Span::new(self.start + 1, self.end - 1);
                        match self.parse_string(&self.src[span.range()], next.lines, 1, false) {
                            Some(string) => {
                                let val = string;
                                Ok(Token::new(
                                    OutSet::String {
                                        value : StringValue::String(val),
                                        set   : StringSet::Byte
                                    },
                                    next.pos,
                                    next.lines
                                ))
                            },
                            None => {
                               Ok(Token::new(
                                   OutSet::String {
                                       value : StringValue::Span(span),
                                       set   : StringSet::Byte
                                   },
                                   next.pos,
                                   next.lines
                               ))
                            }
                        }
                    },

                    StrSet::RawStr     { hashes, err } |
                    StrSet::RawByteStr { hashes, err } => {
                        let hashes = hashes as usize;
                        let add = match set {
                            StrSet::RawByteStr { .. } => 1,
                            _                         => 0,
                        };
                        let span = Span::new(self.start + 2 + hashes + add, self.end - 1 - hashes);
                        if let Ok(()) = self.check_raw_str(err, &next) {
                            Ok(Token::new(
                                OutSet::String {
                                    value : StringValue::Span(span),
                                    set   : StringSet::Normal
                                },
                                next.pos,
                                next.lines
                            ))
                        } else {
                            Err(LexError::LexError)
                        }
                    },
                }
            }

            InSet::Char { closed, set } => {
                if !closed {
                    self.unclosed(true, &next);
                    return Err(LexError::LexError);
                }
                let src = &self.src[next.pos.start + 1..next.pos.end - 1];
                if src.replace('\\', "").chars().count() > 1 {
                    self
                        .msg("Characters cannot have more than one character!", next.lines)
                        .set_code_("E0008")
                        .add_vis_()
                        .add_ptr('^', next.pos)
                        .show();
                    return Err(LexError::LexError);
                }
                match set {
                    CharSet::Normal => {
                        match self.parse_string(src, next.lines, 1, true) {
                            Some(string) => {
                                let val = string;
                                Ok(Token::new(
                                    OutSet::Char {
                                        value : StringValue::String(val),
                                        set   : StringSet::Normal
                                    },
                                    next.pos,
                                    next.lines
                                ))
                            },
                            None => {
                                Ok(Token::new(
                                    OutSet::Char {
                                        value : StringValue::Span(Span::new(self.start + 1, self.end - 1)),
                                        set   : StringSet::Normal
                                    },
                                    next.pos,
                                    next.lines
                                ))
                            }
                        }
                    }

                    CharSet::ByteChar => {
                        // TODO: Highlight the `b` in parse errors
                        match self.parse_string(src, next.lines, 1, true) {
                            Some(string) => {
                                let val = string;
                                Ok(Token::new(
                                    OutSet::Char {
                                        value : StringValue::String(val),
                                        set   : StringSet::Byte
                                    },
                                    Span::new(self.start - 1, self.end),
                                    next.lines
                                ))
                            },
                            None => {
                                Ok(Token::new(
                                    OutSet::Char {
                                        value : StringValue::Span(Span::new(self.start + 1, self.end - 1)),
                                        set   : StringSet::Byte
                                    },
                                    Span::new(self.start - 1, self.end),
                                    next.lines
                                ))
                            }
                        }
                    }
                }
            }

            InSet::Invalid => {
                self
                    .msg(format!("Unexpected character `{}`!", &self.src[next.pos.range()]), next.lines)
                    .set_code_("E0007")
                    .add_vis_()
                    .add_ptr('^', next.pos)
                    .set_ptr_msg("Help: Remove this character", POINTER_HELP_MSG_COLOR)
                    .show();
                Err(LexError::LexError)
            }
        }
    }

    fn unclosed(&self, is_char: bool, next: &InToken) {
        let chr_str = if is_char { "character" } else { "string" };
        let format_info = format!("Info: This {} has not been closed", chr_str);
        let format_help = format!("Help: Add this character to the end of your {}", chr_str);
        self
            .msg(format!("Unclosed {}!", if is_char { "character" } else { "string" }), next.lines)
            .set_code_("E0011")
            .add_vis_()
            .add_ptr('-', next.pos)
            .set_ptr_color(POINTER_INFO_COLOR)
            .set_ptr_msg(&format_info, POINTER_INFO_MSG_COLOR)
            .add_ptr('+', Span::single(next.pos.end))
            .set_ptr_color(POINTER_PLUS_COLOR)
            .set_ptr_insert(next.pos.end, if is_char { "\'" } else { "\"" })
            .set_ptr_msg(&format_help, POINTER_HELP_MSG_COLOR)
            .show()
    }

    fn check_raw_str(&self, err: RawStrErr, next: &InToken) -> Result<(), ()> {
        if let RawStrErr::LacksHashes { lacking, hint } = err {
            let mut string =
                if hint.0 > 0 { String::new() } else { String::from("\"") };
            string += &"#".repeat(lacking);
            let end_pos = if hint.0 == 0 { self.end } else { hint.1 };
            self
                .msg("Unterminated raw string!", next.lines)
                .set_code_("E0009")
                .add_vis_()
                .add_ptr('-', Span::new(next.pos.start, end_pos))
                .set_ptr_color(POINTER_INFO_COLOR)
                .set_ptr_msg("Info: This raw string has not been terminated", POINTER_INFO_MSG_COLOR)
                .add_ptr('+', Span::single(end_pos))
                .set_ptr_color(POINTER_PLUS_COLOR)
                .set_ptr_insert(end_pos, &string)
                .set_ptr_msg("Help: Add these character(s) to the end of your raw string", POINTER_HELP_MSG_COLOR)
                .show();
            return Err(());
        }
        if let RawStrErr::UnexpectedChar { unexpected, location } = err {
            self
                .msg(format!("Unexpected character at the start of raw string `{unexpected}`!"), next.lines)
                .set_code_("E0010")
                .add_vis_()
                .add_ptr('-', Span::new(next.pos.start, location))
                .set_ptr_color(POINTER_INFO_COLOR)
                .set_ptr_msg("Info: This raw string has an invalid character", POINTER_INFO_MSG_COLOR)
                .add_ptr('^', Span::new(location, location + unexpected.len_utf8()))
                .set_ptr_msg("Help: Remove this character", POINTER_HELP_MSG_COLOR)
                .show();
            return Err(());
        }
        if let RawStrErr::ExcessHashes = err {
            self
                .msg(format!("Too many hashes on raw string! Max 255 hashes"), next.lines)
                .set_code_("E0013")
                .add_vis_()
                .add_ptr('^', next.pos)
                .show();
            return Err(());
        }
        Ok(())
    }

    /// Check if the suffix is a float suffix
    #[inline]
    fn is_float_suffix(suffix: &str) -> Option<NumberPrecision> {
        match suffix {
            // Empty value defaults to f32
            ""            => Some(NumberPrecision::Normal),
            "f32" | "F32" => Some(NumberPrecision::Normal),
            "f64" | "F64" => Some(NumberPrecision::Large) ,
            _             => None,
        }
    }

    fn parse_string(&self, string: &str, lines: Span, offset: usize, is_char: bool) -> Option<String> {
        let mut str = Cow::Borrowed(string);
        let mut chars = string.chars();
        let mut pos = 0;
        let mut line = 0;
        loop {
            let char = match chars.next() { Some(v) => v, _ => break };
            pos += char.len_utf8();
            // Add newlines
            if char == '\n' { line += 1; }
            if char == '\\' {
                let prev_pos = pos;
                let next = chars.next();
                pos += next.map_or(0, |v| v.len_utf8());
                // Parse the escape sequence
                let escape = match next {
                    Some('\n') => {
                        line += 1;
                        loop {
                            // Take all the whitespace characters after escaping a newline
                            let mut clone = chars.clone();
                            match clone.next() {
                                Some(c) if c.is_whitespace() => {
                                    if c == '\n' { line += 1; }
                                    pos += c.len_utf8();
                                    chars = clone
                                },
                                _ => break,
                            }
                        }
                        match &mut str {
                            Cow::Borrowed(_) => {
                                // Use prev_pos - 1 to remove the `\` symbol
                                let out_str = string[0..prev_pos - 1].to_owned();
                                str = Cow::Owned(out_str);
                            },
                            Cow::Owned(_) => (),
                        };
                        continue;
                    }
                    // Alarm character
                    Some('a')  => '\x07',
                    // Backspace character
                    Some('b')  => '\x08',
                    // Vertical tab character
                    Some('v')  => '\x0B',
                    // Form feed character
                    Some('f')  => '\x0C',
                    // Escape character
                    Some('e')  => '\x1B',
                    // Newline
                    Some('n')  => '\n',
                    // Tab character
                    Some('t')  => '\t',
                    // Carriage return
                    Some('r')  => '\r',
                    // Empty character
                    Some('0')  => '\0',
                    // Escape characters & strings
                    Some('\'') => '\'',
                    Some('\"') => '\"',
                    // Escape backslashes
                    Some('\\') => '\\',

                    // escape sequences of the format: `\u{n[1..5]}` and `\xnn` are not handled yet

                    // An invalid escape sequence was found, so print an error
                    chr => {
                        let chr = if let Some(chr) = chr { chr } else { '\0' };
                        let start = self.start + prev_pos - 1 + offset;
                        let end = self.start + pos + offset;

                        let format_info = format!(
                            "Info: This {} has {}invalid escape sequence{}",
                            if is_char { "character" } else { "string" },
                            if is_char { "an " } else { "" },
                            if is_char { "" } else { "(s)" },
                        );

                        let help = format!(
                            "Perhaps you should use a raw string literal if you wished for the character(s) `\\{}`",
                            chr
                        );

                        let mut msg = self
                            .msg(format!("Expected a valid escape sequence but found: `\\{}`!", chr), lines)
                            .set_code_("E0012")
                            .add_vis_()

                            .add_ptr('-', Span::new(self.start, start))
                            .set_ptr_color(POINTER_INFO_COLOR)
                            .set_ptr_msg(&format_info, POINTER_INFO_MSG_COLOR)

                            .add_ptr('^', Span::new(start, end))
                            .set_ptr_msg("Help: Try replacing this with a valid escape sequence. Ex: `\\n`", POINTER_HELP_MSG_COLOR)

                            .add_ptr('-', Span::new(end, self.end))
                            .set_ptr_color(POINTER_INFO_COLOR);

                        if !is_char {
                            msg =
                                msg
                                    .add_vis_lines(Span::single(lines.start))
                                    .set_label("Help:")
                                    .add_ptr('+', Span::single(self.start))
                                    .set_ptr_color(POINTER_PLUS_COLOR)
                                    .set_ptr_insert(self.start, "r")
                                    .set_ptr_msg(&help, POINTER_HELP_MSG_COLOR)

                                    .add_vis_lines(Span::single(lines.start + line))
                                    .set_label(" ")
                                    .add_ptr('+', Span::single(start))
                                    .set_ptr_color(POINTER_PLUS_COLOR)
                                    .set_ptr_insert(start, "\\")
                                    .set_ptr_msg("Or, you can escape the backslash character", POINTER_HELP_MSG_COLOR);
                        }
                        msg.show();
                        '\0'
                    }
                };

                match &mut str {
                    // If the string is not owned, it means that we had found no escape sequence
                    // So create an owned string till the current position and push the escape
                    Cow::Borrowed(_) => {
                        // Use prev_pos - 1 to remove the `\` symbol
                        let mut out_str = string[0..prev_pos - 1].to_owned();
                        out_str.push(escape);
                        str = Cow::Owned(out_str);
                    },
                    // If it's already owned then just push the escape sequence
                    Cow::Owned(str) => str.push(escape),
                };
                // We should not add the next character until the next loop, so continue here
                continue;
            }
            // If we have already passed an escape sequence, add it to the string
            match &mut str {
                Cow::Owned(str) => str.push(char),
                Cow::Borrowed(_) => (),
            };
        }
        match str {
            Cow::Owned(str) => Some(str),
            _ => None,
        }
    }

    #[inline]
    fn is_int_suffix(suffix: &str) -> Option<(NumberPrecision, bool)> {
        match suffix {
            // Signed integers
            // Empty value defaults to i32
            ""                => Some((NumberPrecision::Normal, false)),

            "i8"    | "I8"    => Some((NumberPrecision::Small , false)),
            "i16"   | "I16"   => Some((NumberPrecision::Medium, false)),
            "i32"   | "I32"   => Some((NumberPrecision::Normal, false)),
            "i64"   | "I64"   => Some((NumberPrecision::Large , false)),
            "isize" | "ISIZE" => Some((NumberPrecision::Max   , false)),
            // Unsigned integers
            "u8"    | "U8"    => Some((NumberPrecision::Small , true)),
            "u16"   | "U16"   => Some((NumberPrecision::Medium, true)),
            "u32"   | "U32"   => Some((NumberPrecision::Normal, true)),
            "u64"   | "U64"   => Some((NumberPrecision::Large , true)),
            "usize" | "USIZE" => Some((NumberPrecision::Max   , true)),
            _                 => None,
        }
    }

    fn msg<S: Display + Clone>(&self, message: S, span: Span) -> Msg<S, &'a str> {
        Msg::new_span(
            message,
            "./file.mrd",
            self.src,
            span,
            Span::new(self.start, self.end),
            MsgType::Error
        )
    }

    fn msg_type<S: Display + Clone>(
        &self,
        message  : S,
        span     : Span,
        msg_type : MsgType
    )
        -> Msg<S, &'a str>
    {
        Msg::new_span(
            message,
            "./file.mrd",
            self.src,
            span,
            Span::new(self.start, self.end),
            msg_type
        )
    }
}