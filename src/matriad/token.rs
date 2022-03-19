use std::{fmt, fmt::{Display, Formatter}};

/// The token type, that the lexer returns
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Ty {
    Whitespace,      // \t \n \r <space>
    Identifier,      // [a-zA-Z_][a-zA-Z0-9_]*
    Number,          // [0-9]* ('.' [0-9]*)?
    String,          // ".*"
    Char,            // '.'

    // operators
    Plus,            // +
    PlusEqual,       // +=

    Minus,           // -
    MinusEqual,      // -=

    Divide,          // /
    DivideEqual,     // /=

    Multiply,        // *
    MultiplyEqual,   // *=

    Modulo,          // %
    ModuloEqual,     // %=

    Not,             // !
    NotEqual,        // !=

    Equal,           // ==
    Assign,          // =

    Greater,         // >
    GreaterEqual,    // >=

    Lesser,          // <
    LesserEqual,     // <=

    Exponent,        // **
    ExponentEqual,   // **=

    And,             // &&
    BitAnd,          // &
    BitAndEqual,     // &=

    Or,              // ||
    BitOr,           // |
    BitOrEqual,      // |=

    BitLeftShift,    // <<
    BitRightShift,   // >>

    BitXor,          // ^
    BitXorEqual,     // ^=

    Range,           // ..
    Dot,             // .

    Question,        // ?

    // delimiters
    Comma,           // ,
    Colon,           // :
    Semicolon,       // ;
    DoubleColon,     // ::

    LeftBracket,     // [
    RightBracket,    // ]

    RightParen,      // )
    LeftParen,       // (

    LeftBrace,       // {
    RightBrace,      // }

    // invalid
    Invalid,         // anything else at all
}

impl Display for Ty {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// This is the value of the token, comes with the token type
/// multiple types are used to reduce the need allocate, every time a token is generated,
/// resulting in better performance
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum SC<'a> {
    Chr(char),
    Str(&'a str),
    String(String),
    Null,
}

impl Display for SC<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            SC::Chr(c) => write!(f, "'{}'", c),
            SC::Str(s) => write!(f, "\"{}\"", s),
            SC::String(s) => write!(f, "\"{}\"", s),
            SC::Null => write!(f, "Null"),
        }
    }
}

impl Clone for SC<'_> {
    fn clone(&self) -> Self {
        match self {
            SC::Chr(c) => SC::Chr(*c),
            SC::Str(s) => SC::Str(*s),
            SC::String(s) => SC::String(s.clone()),
            _ => SC::Null,
        }
    }
}

/// The token struct.<br/>
/// Stores the type and value of the token.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Token<'a>(pub Ty, pub SC<'a>, pub usize, pub usize);

impl Default for Token<'_> {
    fn default() -> Self {
        Token(Ty::Invalid, SC::Null, 0, 0)
    }
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Token(Type = {}, Value = {}, Start = {}, End = {})",
            self.0, self.1, self.2, self.3
        )
    }
}