use std::borrow::Borrow;
use std::iter::Peekable;
use std::ops::Range;
use std::str::Chars;

use crate::matriad::token::*;

pub struct Lexer<'a> {
    src : &'a str,
    pos : usize,
    it  : Peekable<Chars<'a>>
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Lexer {
            src,
            pos : 0,
            it  : src.chars().peekable(),
        }
    }

    fn take_while(&mut self, mut predicate: impl FnMut(char) -> bool) -> Range<usize> {
        let start = self.pos;
        let mut end = start;
        while let Some(c) = self.it.next_if(|&c| predicate(c)) {
            end += c.len_utf8();
        }
        self.pos = end;
        start..end
    }

    fn adv(&mut self) -> Option<char> {
        let next = self.it.next();
        if let Some(c) = next {
            self.pos += c.len_utf8();
        }
        next
    }

    // unused
    // #[inline]
    // pub fn eof(&mut self) -> bool { self.it.peek().is_none() }

    fn tk(&mut self, ty: Ty) -> Option<Token> {
        let start = self.pos;
        self.adv();
        Some(Token(ty, SC::Null, start, self.pos))
    }

    fn op(&mut self, no_matched: Ty, matched: Ty, check: char) -> Option<Token> {
        let start = self.pos;
        self.adv();
        let mut token = no_matched;
        if self.it.peek() == Some(&check) {
            self.adv();
            token = matched;
        }
        Some(Token(
            token,
            SC::Null,
            start,
            self.pos
        ))
    }

    fn op_tri(
        &mut self,
        no_matched : Ty,
        matched1   : Ty,
        check1     : char,
        matched2   : Ty,
        check2     : char
    ) -> Option<Token> {
        let start = self.pos;
        self.adv();
        let peek = self.it.peek();
        let mut token = no_matched;
        if peek == Some(&check1) {
            self.adv();
            token = matched1;
        } else if peek == Some(&check2) {
            self.adv();
            token = matched2;
        }
        Some(Token(
            token,
            SC::Null,
            start,
            self.pos
        ))
    }

    fn parse_escape(curr_char: char, chars: &mut Peekable<Chars>) -> char {
        match curr_char {
            '\\' => {
                let next = chars.next();
                match next {
                    // basic escapes
                    Some('a')  => '\x07',
                    Some('b')  => '\x08',
                    Some('v')  => '\x0B',
                    Some('f')  => '\x0C',
                    Some('e')  => '\x1B',
                    Some('n')  => '\n',
                    Some('t')  => '\t',
                    Some('r')  => '\r',
                    Some('0')  => '\0',
                    Some('\'') => '\'',
                    Some('\"') => '\"',
                    Some('\\') => '\\',

                    // check for `\xnn` pattern where n is a hexadecimal number
                    Some('x' | 'X') => {
                        let mut tmp = [b'0'; 2];
                        for item in tmp.iter_mut() {
                            // [Error for below] Invalid escape sequence provided, expected
                            // escape of the form \xnn or \unnnn where n is a hex number, but found {x}
                            if let Some(c) = chars.next() {
                                if c.is_digit(10) || matches!(c, 'A'..='F' | 'a'..='f') {
                                    *item = c as u8;
                                } else {
                                    /* TODO: Error here (above) */
                                }
                            } else { /* TODO: Error here (above) */ }
                        }
                        let esc = String::from_utf8_lossy(&tmp);
                        match u32::from_str_radix(esc.borrow(), 16) {
                            Ok(v) => char::from_u32(v).unwrap(),
                            Err(_) => '\0' /* TODO: Error here (above) */
                        }
                    },

                    // check for `\u{n[1, 5]}` pattern where n is a hexadecimal number
                    Some('u' | 'U') => {
                        // [Error for below] Invalid escape sequence provided, expected
                        // escape of the form \u{n[1, 5]} where n is a hex number, but found {x}
                        if let Some('{') = chars.peek() {
                            chars.next();
                            let mut len = 0usize;
                            let mut string = String::with_capacity(5);
                            while let Some(char) = chars.next() {
                                if len >= 5 {
                                    // TODO: Error here (above)
                                    break;
                                }
                                if char == '}' { break; }
                                if !char.is_digit(10) && matches!(char, 'A'..='F' | 'a'..='f') {
                                    /* TODO: Error here (above) */
                                }
                                string.push(char);
                                len += 1;
                            }
                            match u32::from_str_radix(string.as_str(), 16) {
                                Ok(v) => char::from_u32(v).unwrap(),
                                Err(_) => '\0' /* TODO: Error here (above) */
                            }
                        } else {
                            /* TODO: Error, found invalid escape sequence! */
                            '\0'
                        }
                    },

                    // failed to construct a valid escape
                    _ => '\0' /* TODO: Error here (Invalid escape sequence provided) */
                }
            }
            // if no escape, return that character that was extracted
            c => c
        }
    }

    fn parse_string(&mut self, test_char : char) -> (SC<'a>, usize, usize, bool, usize) {
        let start = self.pos;
        self.adv();
        let (mut end, mut count, mut escaped) = (start + 1, 0, false);
        while let Some(c) = self.it.next_if(|&c| c != test_char) {
            if c == '\\' { escaped = true; }
            count += 1;
            end += c.len_utf8();
        }
        self.pos = end;

        let res: &str = &self.src[(start + 1)..end];
        let mut closed = false;
        if self.it.peek() == Some(&test_char) {
            self.adv();
            closed = true;
        }
        if escaped {
            let mut string = String::with_capacity(count);
            let mut chars = res.chars().peekable();
            while let Some(c) = chars.next() {
                string.push(Self::parse_escape(c, &mut chars));
            }
            (SC::String(string), start, self.pos, closed, count)
        } else {
            (SC::Str(res), start, self.pos, closed, count)
        }
    }

    #[inline]
    pub fn next(&mut self) -> Option<Token> {
        let peek = self.it.peek();
        if peek.is_none() { return None; }
        let &char = peek.unwrap();
        match char {
            c if c.is_whitespace() => {
                let start = self.pos;
                self.adv();
                Some(Token(Ty::Invalid, SC::Chr(c), start, self.pos))
            },

            // get strings
            '\"' => {
                let (string, start, end, closed, _) =
                    self.parse_string('\"');
                if !closed {
                    // TODO: Error unclosed string literal!
                }
                Some(Token(Ty::String, string, start, end))
            }

            // get characters
            '\'' => {
                let (string, start, end, closed, count) =
                    self.parse_string('\'');
                if !closed {
                    // TODO: Error unclosed character literal!
                }
                if count > 1 {
                    // TODO: Error characters cannot be more than 1 symbol in length!
                }
                Some(Token(Ty::Char, string, start, end))
            }

            // get numbers
            c if c.is_digit(10) => {
                let mut seen_dot = false;
                let mut clone = self.it.clone();
                let range =
                    self.take_while(|c| {
                        if c == '.' {
                            if seen_dot { return false; }
                            seen_dot = true;
                            return true;
                        }
                        c.is_digit(10)
                    });
                let (start, mut end) = (range.start, range.end);
                if (&self.src[start..end]).ends_with('.') {
                    // can this throw an exception?
                    end -= 1;
                    clone.nth(end - start - 1);
                    self.pos -= 1;
                    self.it = clone;
                }
                Some(Token(Ty::Number, SC::Str(&self.src[start..end]), start, end))
            },

            // get identifiers & keywords
            c if c.is_alphanumeric() || c == '_'  => {
                // allows identifiers such as `919#`, `_~#.`, `ident`, `_'_~_.` etc
                let range = self.take_while(|c|
                        c.is_alphanumeric() ||
                        c == '_'            ||
                        c == '~'            ||
                        c == '#'            ||
                        c == '\''
                );
                let (start, end) = (range.start, range.end);
                Some(Token(
                    Ty::Identifier,
                    SC::Str(&self.src[range]),
                    start,
                    end
                ))
            },

            '.' => self.op(Ty::Dot, Ty::Range, '.'),
            ':' => self.op(Ty::Colon, Ty::DoubleColon, ':'),
            '+' => self.op(Ty::Plus, Ty::PlusEqual, '='),
            '-' => self.op(Ty::Minus, Ty::MinusEqual, '='),
            '=' => self.op(Ty::Assign, Ty::Equal, '='),
            '!' => self.op(Ty::Not, Ty::NotEqual, '='),
            '%' => self.op(Ty::Modulo, Ty::ModuloEqual, '='),
            '^' => self.op(Ty::BitXor, Ty::BitXorEqual, '='),

            '&' => self.op_tri(Ty::BitAnd , Ty::BitAndEqual , '=', Ty::And          , '&'),
            '|' => self.op_tri(Ty::BitOr  , Ty::BitOrEqual  , '=', Ty::Or           , '|'),
            '<' => self.op_tri(Ty::Lesser , Ty::LesserEqual , '=', Ty::BitLeftShift , '<'),
            '>' => self.op_tri(Ty::Greater, Ty::GreaterEqual, '=', Ty::BitRightShift, '>'),

            ';' => self.tk(Ty::Semicolon),
            ',' => self.tk(Ty::Comma),
            '?' => self.tk(Ty::Question),
            '(' => self.tk(Ty::RightParen),
            ')' => self.tk(Ty::LeftParen),
            '[' => self.tk(Ty::RightBrace),
            ']' => self.tk(Ty::LeftBrace),
            '{' => self.tk(Ty::RightBracket),
            '}' => self.tk(Ty::LeftBracket),

            '/' => {
                let start = self.pos;
                self.adv();
                let peek = self.it.peek();
                if let Some(&'/') = peek {
                    // single line comment
                    self.adv();
                    let _ = self.take_while(|c| c != '\n');
                    self.adv();
                    return self.next();
                } else if let Some(&'*') = peek {
                    // multiline comments
                    // TODO: Add an error if the comment is not closed
                    self.adv();
                    let (mut end, mut layer, mut start) = (false, 1usize, false);
                    let _ = self.take_while(|c| {
                        if start && c == '*' { layer += 1; }
                        start = matches!(c, '/');
                        let bool = !(end && c == '/');
                        end = matches!(c, '*');
                        if !bool { layer -= 1; }
                        layer > 0
                    });
                    self.adv();
                    return self.next();
                }
                if let Some(&'=') = peek {
                    self.adv();
                    Some(Token(Ty::DivideEqual, SC::Null, start, self.pos))
                } else {
                    Some(Token(Ty::Divide, SC::Null, start, self.pos))
                }
            },

            '*' => {
                let start = self.pos;
                self.adv();
                let mut token = Ty::Multiply;
                match self.it.peek() {
                    Some(&'=') => {
                        self.adv();
                        token = Ty::MultiplyEqual; // *=
                    },
                    Some(&'*') => {
                        self.adv();
                        if let Some(&'=') = self.it.peek() {
                            self.adv();
                            token = Ty::ExponentEqual; // **=
                        } else {
                            token = Ty::Exponent; // **
                        }
                    }
                    _ => ()  // *
                };
                Some(Token(token, SC::Null, start, self.pos))
            },

            c => {
                let start = self.pos;
                self.adv();
                Some(Token(Ty::Invalid, SC::Chr(c), start, self.pos))
            }
        }
    }
}