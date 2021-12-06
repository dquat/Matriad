use std::borrow::Cow;
use std::sync::{Arc, Mutex};
use std::thread;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Ty {
    Whitespace, // \t \n \r <space>
    Identifier, // [a-zA-Z_][a-zA-Z0-9_]*
    Number, // [0-9]* ('.' [0-9]*)?

    // operators
    Plus, // +
    PlusEqual, // +=

    Minus, // -
    MinusEqual, // -=

    Divide, // /
    DivideEqual, // /=

    Multiply, // *
    MultiplyEqual, // *=

    Modulo, // %
    ModuloEqual, // %=

    Not, // !
    NotEqual, // !=

    Equal, // ==
    Assign, // =

    Greater, // >
    GreaterEqual, // >=

    Lesser, // <
    LesserEqual, // <=

    Exponent, // **
    ExponentEqual, // **=

    And, // &&
    BitAnd, // &
    BitAndEqual, // &=

    Or, // ||
    BitOr, // |
    BitOrEqual, // |=

    BitLeftShift, // <<
    BitRightShift, // >>

    BitXor, // ^
    BitXorEqual, // ^=

    Range, // ..
    Dot, // .

    // delimiters
    Comma, // ,
    Colon, // :
    Semicolon, // ;

    LeftBracket, // [
    RightBracket, // ]

    RightParen, // )
    LeftParen, // (

    LeftBrace, // {
    RightBrace, // }

    // invalid
    Invalid, // anything else at all
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SC {
    C(char),
    S(String),
    Null,
}

pub struct Lexer<'a> {
    pub src   : Cow<'a, str>,
    pub pos   : usize,
    pub chars : Vec<char>,
}

impl<'a> Lexer<'a> {

    pub fn new(src: &'a str) -> Self {
        let mut char_vec = Vec::with_capacity(src.len());
        src
            .chars()
            .for_each(|c|
                char_vec.push(c)
            );
        Lexer {
            src   : Cow::Borrowed(src),
            pos   : 0,
            chars : char_vec,
        }
    }

    #[inline]
    pub fn take_while<F>(&mut self, mut predicate: F) -> String
        where F: FnMut(char) -> bool {
        let mut dist = 0;
        for &c in self.chars.iter().skip(self.pos) {
            if predicate(c) {
                dist += 1;
            } else {
                break;
            }
        };
        let mut string = String::with_capacity(dist);
        for &c in self.chars[self.pos..self.pos + dist].iter() {
            string.push(c)
        }
        self.pos += dist;
        string
    }

    #[inline]
    pub fn eof(&self) -> bool {
        self.pos >= self.chars.len()
    }

    #[inline]
    pub fn op(&mut self, ty: Ty) -> Option<(Ty, SC)> {
        self.pos += 1;
        Some((ty, SC::Null))
    }
    
    #[inline]
    pub fn op_ch(&mut self, ty: Ty, ch_ty: Ty, ch: char) -> Option<(Ty, SC)> {
        self.pos += 1;
        if !self.eof() && self.chars[self.pos] == ch {
            self.pos += 1;
            Some((ch_ty, SC::Null))
        } else {
            Some((ty, SC::Null))
        }
    }

    #[inline]
    pub fn op_eq(&mut self, ty: Ty, eq_ty: Ty) -> Option<(Ty, SC)> {
        self.op_ch(ty, eq_ty, '=')
    }

    #[inline]
    pub fn next(&mut self) -> Option<(Ty, SC)>{
        if self.eof() {
            return None;
        }
        let char = self.chars[self.pos];
        match char {
            c if c.is_whitespace() => {
                self.pos += 1;
                Some((Ty::Whitespace, SC::C(c)))
            },

            c if c.is_digit(10) => {
                let mut seen_dot = false;
                Some((
                    Ty::Number,
                    SC::S(self.take_while(|c| {
                        if c == '.' {
                            if seen_dot { return false; }
                            seen_dot = true;
                            return true;
                        }
                        c.is_digit(10)
                    })),
                ))
            }

            c if c.is_alphabetic() || c == '_' => {
                Some((
                    Ty::Identifier,
                    SC::S(self.take_while(|c| c.is_alphabetic() || c.is_digit(10) || c == '_')),
                ))
            }
            
            '+' => self.op_eq(Ty::Plus, Ty::PlusEqual),
            '-' => self.op_eq(Ty::Minus, Ty::MinusEqual),
            '/' => self.op_eq(Ty::Divide, Ty::DivideEqual),
            '%' => self.op_eq(Ty::Modulo, Ty::ModuloEqual),
            '!' => self.op_eq(Ty::Not, Ty::NotEqual),
            '>' => {
                self.pos += 1;
                if !self.eof() && self.chars[self.pos] == '=' {
                    self.pos += 1;
                    Some((Ty::GreaterEqual, SC::Null)) // >=
                } else if !self.eof() && self.chars[self.pos] == '>' {
                    self.pos += 1;
                    Some((Ty::BitRightShift, SC::Null)) // >>
                } else {
                    Some((Ty::Greater, SC::Null)) // >
                }
            },
            '<' => {
                self.pos += 1;
                if !self.eof() && self.chars[self.pos] == '=' {
                    self.pos += 1;
                    Some((Ty::LesserEqual, SC::Null)) // <=
                } else if !self.eof() && self.chars[self.pos] == '<' {
                    self.pos += 1;
                    Some((Ty::BitLeftShift, SC::Null)) // <<
                } else {
                    Some((Ty::Lesser, SC::Null)) // <
                }
            },
            '=' => self.op_eq(Ty::Assign, Ty::Equal),
            '^' => self.op_eq(Ty::BitXor, Ty::BitXorEqual),

            '*' => {
                self.pos += 1;
                if !self.eof() && self.chars[self.pos] == '=' {
                    self.pos += 1;
                    Some((Ty::MultiplyEqual, SC::Null)) // *=
                } else if !self.eof() && self.chars[self.pos] == '*' {
                    self.pos += 1;
                    if !self.eof() && self.chars[self.pos] == '=' {
                        self.pos += 1;
                        Some((Ty::ExponentEqual, SC::Null)) // **=
                    }
                    else {
                        Some((Ty::Exponent, SC::Null)) // **
                    }
                } else {
                    Some((Ty::Multiply, SC::Null)) // *
                }
            },

            '&' => {
                self.pos += 1;
                if !self.eof() && self.chars[self.pos] == '=' {
                    self.pos += 1;
                    Some((Ty::BitAndEqual, SC::Null)) // &=
                } else if !self.eof() && self.chars[self.pos] == '&' {
                    self.pos += 1;
                    Some((Ty::And, SC::Null)) // &&
                } else {
                    Some((Ty::BitAnd, SC::Null)) // &
                }
            },

            '|' => {
                self.pos += 1;
                if !self.eof() && self.chars[self.pos] == '=' {
                    self.pos += 1;
                    Some((Ty::BitOrEqual, SC::Null)) // |=
                } else if !self.eof() && self.chars[self.pos] == '|' {
                    self.pos += 1;
                    Some((Ty::Or, SC::Null)) // ||
                } else {
                    Some((Ty::BitOr, SC::Null)) // |
                }
            },

            '{' => self.op(Ty::LeftBrace),
            '}' => self.op(Ty::RightBrace),
            '(' => self.op(Ty::LeftParen),
            ')' => self.op(Ty::RightParen),
            '[' => self.op(Ty::LeftBracket),
            ']' => self.op(Ty::RightBracket),

            ';' => self.op(Ty::Semicolon),
            ':' => self.op(Ty::Colon),
            ',' => self.op(Ty::Comma),

            '.' => {
                self.pos += 1;
                if !self.eof() && self.chars[self.pos] == '.' {
                    self.pos += 1;
                    Some((Ty::Range, SC::Null))
                } else {
                    Some((Ty::Dot, SC::Null))
                }
            }

            c => {
                self.pos += 1;
                Some((Ty::Invalid, SC::C(c)))
            },
        }
    }
}