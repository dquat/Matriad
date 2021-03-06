use std::{
    fmt,
    fmt::{
        Display,
        Formatter,
    },
};

use crate::matriad::util::Span;

/// The comment type of a comment. Either a doc comment or a normal one.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CommentSet {
    /// /// -comment- \n or /** -comment- */
    Doc,
    /// a normal comment: // -comment- or /* -comment- */
    None,
}

/// The type of number that has been lexed
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NumberSet {
    /// 0b0001
    Binary,
    /// 0o0001
    Octal,
    /// 0xFF09
    Hex,
    /// 1.9, 11, 0.9
    Normal,
}

impl Display for NumberSet {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let str = match self {
            Self::Hex    => "Hexadecimal",
            Self::Binary => "Binary",
            Self::Normal => "Normal",
            Self::Octal  => "Octal",
        };
        write!(f, "{str}")
    }
}

/// An enum to define the type of character the lexer has lexed
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CharSet {
    /// A special character that's a number, but looks like a character: `b'c', b'h', b'a', b'r'`
    ByteChar,
    /// A normal character: `'c'`
    Normal,
}

/// An error class to store raw string errors
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RawStrErr {
    /// Unexpected character between `r#` and `"`, i.e. something like `r#["Oh no!"#`
    UnexpectedChar {
        /// The character that was unexpected
        unexpected : char,
        /// The place were this character was found
        location   : usize,
    },
    /// Less hashes on the right than what was given on the left: `r###"This is an error?"##`
    LacksHashes {
        /// How many hashes is lacked
        lacking : usize,
        /// This is what the lexer guesses could be the ending of the raw string
        /// 0. Maximum number of hashes that were encountered but could not close the string
        /// When it is `0`, it means that no hashes were found anywhere after the start of the raw
        /// string
        /// 1. The position, where these hashes started
        hint    : (usize, usize)
    },
    /// More than 255 hashes provided. I mean, would you need more than 255 hashes in a raw string?
    /// Come on, nobody needs it
    ExcessHashes,
    /// No error whatsoever when parsing a raw string
    NoError,
}

/// An enum to define the type of string the lexer has lexed
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StrSet {
    /// A raw string where escape sequences loose their powers: `r#"str"#, r"hello\", r##"#"##`
    RawStr {
        /// The number of hashes. Maximum hashes must be u8::MAX
        hashes : u8,
        /// Any errors encountered by the lexer when trying to lex the given raw string
        err    : RawStrErr
    },
    /// A special raw string that does what [`StrSet::RawStr`] does and transforms
    /// it's characters into an array of numbers: `rb#"str"#, br"hello\", rb##"#"##`
    RawByteStr {
        /// The number of hashes. Maximum hashes must be u8::MAX
        hashes : u8,
        /// Any errors encountered by the lexer when trying to lex the given raw string
        err    : RawStrErr
    },
    /// A byte string that transforms it's characters into an array of numbers:
    /// `b"abc", b"str", b"hello"`
    ByteStr {
        /// Weather or not the byte string was closed
        closed : bool
    },
    /// A very, very normal string: `"normal, completely normal"`, quite possibly too normal
    Normal {
        /// Weather or not the normal string was closed
        closed : bool
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Delimiters {
    // operators
    /// +
    Plus,
    /// +=
    PlusEqual,

    /// -
    Minus,
    /// -=
    MinusEqual,

    /// /
    Divide,
    /// /=
    DivideEqual,

    /// *
    Multiply,
    /// *=
    MultiplyEqual,

    /// %
    Modulo,
    /// %=
    ModuloEqual,

    /// !
    Not,
    /// !=
    NotEqual,

    /// ==
    Equal,
    /// =
    Assign,

    /// >
    Greater,
    /// >=
    GreaterEqual,

    /// <
    Lesser,
    /// <=
    LesserEqual,

    /// **
    Exponent,
    /// **=
    ExponentEqual,

    /// &&
    And,
    /// &
    BitAnd,
    /// &=
    BitAndEqual,

    /// ||
    Or,
    /// |
    BitOr,
    /// |=
    BitOrEqual,

    /// <<
    BitLeftShift,
    /// <<=
    BitLeftShiftEqual,

    /// >>
    BitRightShift,
    /// >>=
    BitRightShiftEqual,

    /// ^
    BitXor,
    /// ^=
    BitXorEqual,

    /// A range operator: `..`
    Range,
    /// A dot character: `.`
    Dot,

    /// A question mark: `?`
    Question,

    // delimiters
    /// An at character `@`
    At,
    /// A hash character: `#`
    Hash,
    /// A comma: `,`
    Comma,
    /// A tilde character: `~`
    Tilde,
    /// A Colon: `:`
    Colon,
    /// A Dollar sign: `$`
    Dollar,
    /// A backtick: `\`\`
    BackTick,
    /// A semicolon: `;`
    Semicolon,
    /// A double colon: `::`
    DoubleColon,

    /// An opening bracket: `[`
    LeftBracket,
    /// A closing bracket: `]`
    RightBracket,

    /// An opening parenthesis: `)`
    RightParen,
    /// A closing parenthesis: `(`
    LeftParen,

    /// An opening flower bracket or brace: `{`
    LeftBrace,
    /// A closing flower bracket or brace: `}`
    RightBrace,
}

pub mod lexer {
    use super::*;

    /// The token type, that the lexer returns
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub enum Set {
        /// \t \n \r <space> and other special unicode spaces.
        Whitespace,

        /// Any valid identifier, this is currently open to more symbols than other languages:
        /// `x`, `very~nice`, `token#9`, `no_no_no`, `____so_hard_to#find~me`
        /// The `#` and `~` symbols may be removed from identifiers when the language progresses
        /// and cannot accommodate such symbols as valid identifiers
        Identifier,

        /// A float, one that must have no trailing, or preceding dots. Exponents can be used
        ///
        /// Valid: `1.0`, `0.1`, `10e10`, `1.299e-99` etc.
        ///
        /// Invalid: "`1.`" -> ("`1`" & "`.`"), "`.1`" -> ("`.`" & "`1`")
        ///
        /// The `number` property holds the range of characters where the actual number is present
        /// It holds values like: `1.422`, `0.01` etc.
        ///
        /// The `exponent` property holds the range of characters where the exponent factor is present
        /// It holds values like: `+20`, `-19`, `4` etc.
        Float {
            /// The type of float
            set          : NumberSet,
            /// Whether or not a number is empty, i.e. it has an empty exponent
            empty        : bool,
            /// The number value of the floating point number
            number       : Span,
            /// The span of the exponent, if any. If there's no exponent, the start and end locations
            /// will be the same value
            exponent     : Span,
            /// The start location of the suffix i.e it would be 3 in `1.0usize`
            suffix_start : usize,
        },

        /// An integer, this can be of any type, hex, octal, binary, or a normal integer.
        Int {
            /// The type of integer
            set          : NumberSet,
            /// Whether or not an integer is empty, i.e 0x | 0b | 0o
            empty        : bool,
            /// The start location of the suffix i.e it would be 1 in `1usize`
            suffix_start : usize,
        },

        /// Any of the string types defined in [`StrSet`]
        String { set: StrSet },

        /// Any of the character types defined in [`CharSet`]
        Char { closed: bool, set: CharSet },

        // comments
        /// A multiline comment that supports nested comments
        /// Normal comments: `/* comment text\n comment text... */` & `/**/`
        ///
        /// Nesting: `/* /* A valid & nested comment */ */`,
        /// `/* /* An invalid, nested comment that has not been closed */`
        ///
        /// Doc comments: `/** A multiline\n doc comment */`
        MultiLineComment { set: CommentSet, closed: bool, depth: usize },

        /// Single line comments
        ///
        /// Normal comments: `// <comment text>\n`
        SingleLineComment { set: CommentSet },

        /// Delimiters and operators
        Delimiter (Delimiters),

        /// An invalid character has been lexed by the token.
        /// This will produce an error in future stages of compilation
        Invalid,
    }

    impl Display for Set {
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            write!(f, "{:?}", self)
        }
    }
}

/// This is for the lexer error validator
pub mod lexthrow {
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum StringValue {
        /// The value of a string that has been escaped and been parsed
        String(String),
        /// The value of a string that has no escape sequences
        Span  (Span),
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum StringSet {
        /// A normal character / string or raw string
        Normal,
        /// A byte string or character
        Byte,
        // Raw type is not required here since there's nothing different about it when compared to
        // the normal string, for the parser
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum NumberKind {
        /// A floating point number
        Float,
        /// An unsigned integer, i.e. cannot be negative
        UnsignedInteger,
        /// A normal integer
        Integer,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum NumberPrecision {
        /// A u8 or i8
        Small  = 8,
        /// A u16 or i16
        Medium = 16,
        /// A u32, i32 or f32
        Normal = 32,
        /// A u64, i64 or f64
        Large  = 64,
        /// A u128
        Full   = 128,
        /// A usize
        Max,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum Set {
        /// Strings
        String {
            /// The value of a string
            value : StringValue,
            /// The type of string
            set   : StringSet,
        },
        /// Characters
        Char   {
            /// The value of a character
            value : StringValue,
            /// The type of character
            set   : StringSet,
        },
        /// Numbers
        Number {
            /// The type of number, i.e. a floating point number, a unsigned integer, or a normal,
            /// signed integer
            kind      : NumberKind,
            /// The precision of the number, basically the same things as Rust's number type system
            precision : NumberPrecision,
            /// The type of the number, weather a hexadecimal one, a binary one, an octal or just a
            /// normal number
            set       : NumberSet,
            /// The exponent of a number, this can also be a negative exponent
            exponent  : isize,
        },
        /// Delimiters
        Delimiter (Delimiters),
        /// Identifiers and keywords, what else?
        Identifier,
    }

    impl Display for Set {
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            write!(f, "{:?}", self)
        }
    }
}

/// The token struct
///
/// Stores the type and location data of each token so that it can later by analyzed by the parser
/// or any other later step used in compiling the code
#[derive(Debug, Clone, PartialEq)]
pub struct Token<S> {
    /// The token type, here called a set because parts of my keyboard is broken and it's harder to
    /// type in "token_type" than "set"
    pub set   : S,
    /// The position indexes referenced by the token
    pub pos   : Span,
    /// All the lines which this token spans
    pub lines : Span,
}

impl<S> Token<S> {
    /// Create a new token
    pub fn new(set: S, pos: Span, lines: Span) -> Self {
        Self { set, pos, lines }
    }

    /// Format the token into something *more* readable than a debug print
    pub fn show(&self, src: &str) -> String
        where S: Display {
        format!(
            "Token(Type = {}, Value = {:?}, Pos = {}, Line = {})",
            self.set, self.pos.as_src(src), self.pos, self.lines,
        )
    }
}

impl<S: Default> Default for Token<S> {
    fn default() -> Self {
        Token::new(S::default(), Span::new(0, 0), Span::new(0, 0))
    }
}

impl<S: Display> Display for Token<S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Token(Type = {}, Pos = {}, Line = {})",
            self.set, self.pos, self.lines,
        )
    }
}