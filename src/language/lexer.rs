#[path="../language/token.rs"]
mod token;

// imports
use std::borrow::{Borrow, Cow};
use std::iter::Peekable;
use std::ops::Range;
use std::str::{CharIndices, Chars};
use /*crate::language::*/token::{Token, SC, Ty};

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    pub src   : Cow<'a, str>,
    pub it    : Peekable<CharIndices<'a>>,
    pub next  : Option<Token<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src   : Cow::Borrowed(src),
            it    : src.char_indices().peekable(),
            next  : None,
        }
    }

    #[inline]
    fn get_start(&self, value: Option<(usize, char)>) -> usize {
        if let Some((i, _)) = value { i }
        else { self.src.len() }
    }

    #[inline]
    fn take_while_range<F>(&mut self, mut predicate: F) -> Range<usize>
        where F: FnMut(char) -> bool {
        let start = match self.it.next_if(|&(_, c)| predicate(c)) {
            Some((i, _)) => i,
            None => unreachable!(),
        };
        let mut end = start;
        while let Some((i, _)) = self.it.next_if(|&(_, c)| predicate(c)) {
            end = i + 1;
        }
        start..end
    }

    #[inline]
    fn find(string: &str, chr: u8) -> bool {
        let ptr = string.as_ptr();
        let mut found = false;
        for i in 0..string.len() {
            let b = unsafe { *ptr.add(i) };
            if b == chr {
                found = true;
                break;
            }
        }
        found
    }

    fn take_while<F>(&mut self, mut predicate: F) -> &str
        where F: FnMut(char) -> bool {
        let range = self.take_while_range(&mut predicate);
        &self.src[range]
    }

    #[inline]
    pub fn eof(&mut self) -> bool {
        self.it.peek().is_none() && self.next.is_none()
    }

    fn tk(&mut self, ty: Ty, item: SC<'a>, len: usize) -> Option<Token> {
        let next = self.it.next();
        let start = self.get_start(next);
        Some(Token(ty, item, start, start + len))
    }

    #[inline] // len is always 1 in this lexer, where this function is used
    fn op(&mut self, ty: Ty) -> Option<Token> {
        self.tk(ty, SC::Null, 1)
    }

    #[inline]
    fn check(peek: Option<(usize, char)>, value: char) -> bool {
        peek.is_some() && peek.unwrap().1 == value
    }

    fn op_di(&mut self, ty: Ty, ch: char, match_ty: Ty) -> Option<Token> {
        let next = self.it.next();
        let peek = self.it.peek().copied();
        if Self::check(peek, ch) {
            self.it.next();
            let i = next.unwrap().0;
            Some(Token(match_ty, SC::Null, i, i + 2))
        } else {
            let start = self.get_start(next);
            Some(Token(ty, SC::Null, start, start + 1))
        }
    }

    fn op_eq(&mut self, ty: Ty, ty_eq: Ty) -> Option<Token> {
        self.op_di(ty, '=', ty_eq)
    }

    fn op_tri(&mut self, ty: Ty, ty1: Ty, ch1: char, ty2: Ty, ch2: char) -> Option<Token> {
        let next = self.it.next();
        let start = self.get_start(next);
        let peek = self.it.peek().copied();
        if Self::check(peek, ch1) {
            self.it.next();
            Some(Token(ty1, SC::Null, start, start + 2))
        } else if Self::check(peek, ch2) {
            self.it.next();
            Some(Token(ty2, SC::Null, start, start + 2))
        } else {
            Some(Token(ty, SC::Null, start, start + 1))
        }
    }

    fn parse_escape(curr_char: char, chars: &mut Chars) -> char {
        let parse_x_len = |tmp: &mut [u8], chars: &mut Chars| {
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
            let esc = String::from_utf8_lossy(tmp);
            match u32::from_str_radix(esc.borrow(), 16) {
                Ok(v) => char::from_u32(v).unwrap(),
                Err(_) => '\0' /* TODO: Error here (above) */
            }
        };
        match curr_char {
            '\\' => {
                let next = chars.next();
                match next {
                    // basic escapes
                    Some('n')  => '\n',
                    Some('t')  => '\t',
                    Some('r')  => '\r',
                    Some('0')  => '\0',
                    Some('\'') => '\'',
                    Some('\"') => '\"',
                    Some('\\') => '\\',
                    // check for `\xnn` pattern where n is a hexadecimal number
                    Some('x') | Some('X') => parse_x_len(&mut [b'0'; 2], chars),
                    // check for `\unnnn` pattern where n is a hexadecimal number
                    Some('u') | Some('U') => parse_x_len(&mut [b'0'; 4], chars),
                    // failed to construct a valid escape
                    _ => '\0' /* TODO: Error here (Invalid escape sequence provided) */
                }
            }
            // if no escape, return that character that was extracted
            c => c
        }
    }

    fn parse_string(&mut self, test_char: char) -> (SC<'_>, usize, usize, usize) {
        let next = self.it.next();
        let start = self.get_start(next);
        // have to shadow res in order to keep the compiler happy
        let res = self.take_while_range(|c| c != test_char);
        let next = self.it.next();
        let res: &str = &self.src[res];
        let mut closed = 1;
        if next.is_none() || next.unwrap().1 != test_char {
            /* TODO: Error here (Unclosed character) */
            closed = 0;
        }
        if Self::find(res, b'\\') {
            let mut string = String::with_capacity(res.len());
            let mut chars = res.chars();
            let count = res.chars().count();
            let mut len = 0;
            while let Some(c) = chars.next() {
                len += 1;
                string.push(Self::parse_escape(c, &mut chars));
            }
            // if escape sequences exist, allocate a string (lowers performance)
            (SC::String(string), len, start, 1 + start + count + closed)
        } else {
            let count = res.chars().count();
            // if no escape sequences exist,
            // a string doesn't need to be allocated (better performance)
            (SC::Str(res), count, start, 1 + start + count + closed)
        }
    }

    #[inline]
    pub fn next(&mut self) -> Option<Token> {
        // get the next token (generated only when numbers end with a trailing dot / range)
        if let Some(Token(ty, sc, s, e)) = &self.next {
            let (ty, sc, s, e) = (*ty, sc.clone(), *s, *e);
            self.next = None;
            return Some(Token(ty, sc, s, e));
        }
        if self.eof() { return None; }
        let char = self.it.peek().unwrap().1;
        match char {
            // get whitespace
            c if c.is_whitespace() => self.tk(Ty::Whitespace, SC::Chr(c), 1),

            // get strings and chars
            '\"' => {
                let (string, _, start, end) = self.parse_string('\"');
                Some(Token(Ty::String, string, start, end))
            },

            '\'' => {
                let (char, len, start, end) = self.parse_string('\'');
                if len != 1 {
                    /* TODO: Error here (Expected a single character, found {x} characters) */
                }
                Some(Token(Ty::Char, char, start, end))
            },

            // get numbers
            c if c.is_digit(10) => {
                let mut seen_dot = false;
                let range =
                    self.take_while_range(|c| {
                        if c == '.' {
                            if seen_dot { return false; }
                            seen_dot = true;
                            return true;
                        }
                        c.is_digit(10)
                    });
                let (start, mut end) = (range.start, range.end);
                if (&self.src[start..end]).ends_with('.') {
                    end -= 1;
                    let peek = self.it.peek().copied();
                    if Self::check(peek, '.') {
                        self.it.next();
                        self.next = Some(Token(Ty::Range, SC::Null, end, end + 2));
                    } else {
                        self.next = Some(Token(Ty::Dot, SC::Null, end, end + 1));
                    }
                }
                Some(Token(Ty::Number, SC::Str(&self.src[start..end]), start, end))
            },

            // get identifiers & keywords
            c if c.is_alphanumeric() || c == '_' || c == '~' || c == '#'  => {
                // allows identifiers such as `#919`, `_~#.`, `ident`, `_'_"_~_.` etc
                let range = self.take_while_range(|c|
                        c.is_alphanumeric() ||
                        c == '_'            ||
                        c == '~'            ||
                        c == '#'            ||
                        c == '.'            ||
                        c == '\''           ||
                        c == '\"'
                );
                let (start, end) = (range.start, range.end);
                Some(Token(
                    Ty::Identifier,
                    SC::Str(&self.src[range]),
                    start,
                    end
                ))
            },

            // other operators
            '.' => self.op_di(Ty::Dot  , '.', Ty::Range      ),
            ':' => self.op_di(Ty::Colon, ':', Ty::DoubleColon),

            '!' => self.op_eq(Ty::Not   , Ty::NotEqual   ),
            '+' => self.op_eq(Ty::Plus  , Ty::PlusEqual  ),
            '-' => self.op_eq(Ty::Minus , Ty::MinusEqual ),

            '/' => { // divide (/) or, divide equal (/=) comment ( // xx \n or /* xx */ )
                // comments are currently incomplete (sorta)
                let next = self.it.next();
                let peek = self.it.peek().copied();
                // let start = self.get_start_n(next);
                if Self::check(peek, '/') {
                    // single line comment
                    self.it.next();
                    let _ = self.take_while_range(|c| c != '\n');
                    self.it.next();
                    return self.next();
                } else if Self::check(peek, '*') {
                    // multiline comments
                    // TODO: Add an error if the comment is not closed
                    self.it.next();
                    let mut end = false;
                    let mut layer = 1;
                    let mut start = false;
                    let _ = self.take_while_range(|c| {
                        if start && c == '*' { layer += 1; }
                        start = matches!(c, '/');
                        let bool = !(end && c == '/');
                        end = matches!(c, '*');
                        if !bool { layer -= 1; }
                        layer > 0
                    });
                    self.it.next();
                    return self.next();
                }
                let start = self.get_start(next);
                if Self::check(peek, '=') {
                    self.it.next();
                    Some(Token(Ty::DivideEqual, SC::Null, start, start + 2))
                } else {
                    Some(Token(Ty::Divide, SC::Null, start, start + 1))
                }
            },

            '%' => self.op_eq(Ty::Modulo, Ty::ModuloEqual),
            '^' => self.op_eq(Ty::BitXor, Ty::BitXorEqual),
            '=' => self.op_eq(Ty::Assign, Ty::Equal      ),

            '|' => self.op_tri(Ty::BitOr , Ty::BitOrEqual , '=', Ty::Or , '|'),
            '&' => self.op_tri(Ty::BitAnd, Ty::BitAndEqual, '=', Ty::And, '&'),
            '<' => self.op_tri(Ty::Lesser , Ty::LesserEqual , '=', Ty::BitLeftShift , '<'),
            '>' => self.op_tri(Ty::Greater, Ty::GreaterEqual, '=', Ty::BitRightShift, '>'),

            ';' => self.op(Ty::Semicolon),
            '?' => self.op(Ty::Question),
            ',' => self.op(Ty::Comma),

            '{' => self.op(Ty::LeftBrace),
            '}' => self.op(Ty::RightBrace),
            '(' => self.op(Ty::LeftParen),
            ')' => self.op(Ty::RightParen),
            '[' => self.op(Ty::LeftBracket),
            ']' => self.op(Ty::RightBracket),

            '*' => {
                let next = self.it.next();
                let peek = self.it.peek().copied();
                let start = self.get_start(next);
                match peek {
                    p if Self::check(p, '=') => {
                        self.it.next();
                        Some(Token(Ty::MultiplyEqual, SC::Null, start, start + 2))  // *=
                    },
                    p if Self::check(p, '*') => {
                        self.it.next();
                        let peek = self.it.peek().copied();
                        if Self::check(peek, '=') {
                            self.it.next();
                            Some(Token(Ty::ExponentEqual, SC::Null, start, start + 3))  // **=
                        } else {
                            Some(Token(Ty::Exponent, SC::Null, start, start + 2))  // **
                        }
                    }
                    _ => Some(Token(Ty::Multiply, SC::Null, start, start + 1))  // *
                }
            },

            c => {
                // TODO: Add an error here
                self.tk(Ty::Invalid, SC::Chr(c), 1)
            },
        }
    }
}
