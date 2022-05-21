// imports
use crate::matriad::{
    lexer::Lexer,
    token::{
        lexer:: {
            Set,
            Set::*,
        },
        Delimiters::*,
        CommentSet,
        RawStrErr,
        NumberSet,
        CharSet,
        StrSet,
        Token,
    },
    util::Span,
};

type OutToken = Token<Set>;

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Should you really be here?                                          *
 * These are just tests. They are of no importance to you... are they? *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

// Test the lexer
#[cfg(test)]
mod lexer_test {
    use super::*;

    fn run_lexer(src: &str) -> Vec<OutToken> {
        let mut lexer = Lexer::new(src);
        let mut vec = Vec::new();
        while let Some(next) = lexer.next() {
            vec.push(next);
        };
        vec
    }

    fn check_eq(arr: Vec<OutToken>, slice: &[Set]) {
        assert_eq!(arr.len(), slice.len());
        for i in 0..arr.len() {
            assert_eq!(arr[i].set, slice[i]);
        }
    }

    #[test]
    fn normal_integer() {
        let src = "19";
        check_eq(run_lexer(src), &[
            Int {
                set          : NumberSet::Normal,
                empty        : false,
                suffix_start : 2
            }
        ]);
    }


    #[test]
    fn normal_integer_suffix() {
        let src = "19usize";
        check_eq(run_lexer(src), &[
            Int {
                set          : NumberSet::Normal,
                empty        : false,
                suffix_start : 2
            }
        ]);
    }

    #[test]
    fn normal_integer_exponent_as_float() {
        let src = "19e10";
        check_eq(run_lexer(src), &[
            Float {
                set          : NumberSet::Normal,
                empty        : false,
                number       : Span::new(0, 2),
                exponent     : Span::new(3, 5),
                suffix_start : 5
            }
        ]);
    }

    #[test]
    fn normal_integer_exponent_as_float_sign() {
        let src = "19e-10";
        check_eq(run_lexer(src), &[
            Float {
                set          : NumberSet::Normal,
                empty        : false,
                number       : Span::new(0, 2),
                exponent     : Span::new(3, 6),
                suffix_start : 6
            }
        ]);
    }

    #[test]
    fn normal_integer_exponent_as_float_suffix() {
        let src = "19e10f32";
        check_eq(run_lexer(src), &[
            Float {
                set          : NumberSet::Normal,
                empty        : false,
                number       : Span::new(0, 2),
                exponent     : Span::new(3, 5),
                suffix_start : 5
            }
        ]);
    }

    #[test]
    fn normal_integer_exponent_as_float_sign_suffix() {
        let src = "19e-10f32";
        check_eq(run_lexer(src), &[
            Float {
                set          : NumberSet::Normal,
                empty        : false,
                number       : Span::new(0, 2),
                exponent     : Span::new(3, 6),
                suffix_start : 6
            }
        ]);
    }

    #[test]
    fn normal_integer_dot() {
        let src = "19.";
        check_eq(run_lexer(src), &[
            Int {
                set          : NumberSet::Normal,
                empty        : false,
                suffix_start : 2
            },
            Delimiter(Dot),
        ]);
    }

    #[test]
    fn dot_normal_integer() {
        let src = ".19";
        check_eq(run_lexer(src), &[
            Delimiter(Dot),
            Int {
                set          : NumberSet::Normal,
                empty        : false,
                suffix_start : 3
            },
        ]);
    }

    #[test]
    fn octal() {
        let src = "0o007";
        check_eq(run_lexer(src), &[
            Int {
                set          : NumberSet::Octal,
                empty        : false,
                suffix_start : 5
            }
        ]);
    }

    #[test]
    fn octal_suffix() {
        let src = "0o007i16";
        check_eq(run_lexer(src), &[
            Int {
                set          : NumberSet::Octal,
                empty        : false,
                suffix_start : 5
            }
        ]);
    }

    #[test]
    fn empty_octal() {
        let src = "0o";
        check_eq(run_lexer(src), &[
            Int {
                set          : NumberSet::Octal,
                empty        : true,
                suffix_start : 2
            }
        ]);
    }

    #[test]
    fn binary() {
        let src = "0b017";
        check_eq(run_lexer(src), &[
            Int {
                set          : NumberSet::Binary,
                empty        : false,
                suffix_start : 5
            }
        ]);
    }

    #[test]
    fn binary_suffix() {
        let src = "0b907i32";
        check_eq(run_lexer(src), &[
            Int {
                set          : NumberSet::Binary,
                empty        : false,
                suffix_start : 5
            }
        ]);
    }

    #[test]
    fn binary_octal() {
        let src = "0b";
        check_eq(run_lexer(src), &[
            Int {
                set          : NumberSet::Binary,
                empty        : true,
                suffix_start : 2
            }
        ]);
    }

    #[test]
    fn hex() {
        let src = "0xF1E";
        check_eq(run_lexer(src), &[
            Int {
                set          : NumberSet::Hex,
                empty        : false,
                suffix_start : 5
            }
        ]);
    }

    #[test]
    fn hex_suffix() {
        let src = "0xCE7i32";
        check_eq(run_lexer(src), &[
            Int {
                set          : NumberSet::Hex,
                empty        : false,
                suffix_start : 5
            }
        ]);
    }

    #[test]
    fn hex_empty() {
        let src = "0x";
        check_eq(run_lexer(src), &[
            Int {
                set          : NumberSet::Hex,
                empty        : true,
                suffix_start : 2
            }
        ]);
    }

    #[test]
    fn float() {
        let src = "1.9999999";
        check_eq(run_lexer(src), &[
            Float {
                set          : NumberSet::Normal,
                empty        : false,
                number       : Span::new(0, 9),
                exponent     : Span::new(9, 9),
                suffix_start : 9
            }
        ]);
    }

    #[test]
    fn float_exponent() {
        let src = "1.9999999e10";
        check_eq(run_lexer(src), &[
            Float {
                set          : NumberSet::Normal,
                empty        : false,
                number       : Span::new(0, 9),
                exponent     : Span::new(10, 12),
                suffix_start : 12
            }
        ]);
    }

    #[test]
    fn float_exponent_empty() {
        let src = "1.9999999e";
        check_eq(run_lexer(src), &[
            Float {
                set          : NumberSet::Normal,
                empty        : true,
                number       : Span::new(0, 9),
                exponent     : Span::new(10, 10),
                suffix_start : 10
            }
        ]);
    }

    #[test]
    fn float_exponent_sign() {
        let src = "1.9999999e-10";
        check_eq(run_lexer(src), &[
            Float {
                set          : NumberSet::Normal,
                empty        : false,
                number       : Span::new(0, 9),
                exponent     : Span::new(10, 13),
                suffix_start : 13
            }
        ]);
    }

    #[test]
    fn float_exponent_sign_empty() {
        let src = "1.9999999e-";
        check_eq(run_lexer(src), &[
            Float {
                set          : NumberSet::Normal,
                empty        : true,
                number       : Span::new(0, 9),
                exponent     : Span::new(10, 11),
                suffix_start : 11
            }
        ]);
    }

    #[test]
    fn float_suffix() {
        let src = "1.9999999f64";
        check_eq(run_lexer(src), &[
            Float {
                set          : NumberSet::Normal,
                empty        : false,
                number       : Span::new(0, 9),
                exponent     : Span::new(9, 9),
                suffix_start : 9
            }
        ]);
    }

    #[test]
    fn float_exponent_suffix() {
        let src = "1.9999999e10f32";
        check_eq(run_lexer(src), &[
            Float {
                set          : NumberSet::Normal,
                empty        : false,
                number       : Span::new(0, 9),
                exponent     : Span::new(10, 12),
                suffix_start : 12
            }
        ]);
    }

    #[test]
    fn float_exponent_empty_suffix() {
        let src = "1.9999999ef32";
        check_eq(run_lexer(src), &[
            Float {
                set          : NumberSet::Normal,
                empty        : true,
                number       : Span::new(0, 9),
                exponent     : Span::new(10, 10),
                suffix_start : 10
            }
        ]);
    }

    #[test]
    fn float_exponent_sign_suffix() {
        let src = "1.9999999e-10f64";
        check_eq(run_lexer(src), &[
            Float {
                set          : NumberSet::Normal,
                empty        : false,
                number       : Span::new(0, 9),
                exponent     : Span::new(10, 13),
                suffix_start : 13
            }
        ]);
    }

    #[test]
    fn whitespace() {
        let src = " \r\t\n\u{000B}\u{000C}\u{0085}\u{2028}\u{2029}";
        // \u{0085} is 2 characters long?
        // \u{2028} is 3 characters long?
        // \u{2029} is 3 characters long?
        check_eq(run_lexer(src), &[Whitespace]);
    }

    #[test]
    fn delimiters() {
        let src =
            "+-/%*(=){==}&=&&|||=&|**?*=;**=:+=-=!!=%=#~`[]</=<=,<<>>.<<=>>=..::$@^^=";
        check_eq(run_lexer(src), &[
            Delimiter(Plus),
            Delimiter(Minus),
            Delimiter(Divide),
            Delimiter(Modulo),
            Delimiter(Multiply),
            Delimiter(RightParen),
            Delimiter(Assign),
            Delimiter(LeftParen),
            Delimiter(RightBrace),
            Delimiter(Equal),
            Delimiter(LeftBrace),
            Delimiter(BitAndEqual),
            Delimiter(And),
            Delimiter(Or),
            Delimiter(BitOrEqual),
            Delimiter(BitAnd),
            Delimiter(BitOr),
            Delimiter(Exponent),
            Delimiter(Question),
            Delimiter(MultiplyEqual),
            Delimiter(Semicolon),
            Delimiter(ExponentEqual),
            Delimiter(Colon),
            Delimiter(PlusEqual),
            Delimiter(MinusEqual),
            Delimiter(Not),
            Delimiter(NotEqual),
            Delimiter(ModuloEqual),
            Delimiter(Hash),
            Delimiter(Tilde),
            Delimiter(BackTick),
            Delimiter(RightBracket),
            Delimiter(LeftBracket),
            Delimiter(Lesser),
            Delimiter(DivideEqual),
            Delimiter(LesserEqual),
            Delimiter(Comma),
            Delimiter(BitLeftShift),
            Delimiter(BitRightShift),
            Delimiter(Dot),
            Delimiter(BitLeftShiftEqual),
            Delimiter(BitRightShiftEqual),
            Delimiter(Range),
            Delimiter(DoubleColon),
            Delimiter(Dollar),
            Delimiter(At),
            Delimiter(BitXor),
            Delimiter(BitXorEqual),
        ]);
    }

    #[test]
    fn identifiers() {
        let src = "_i~~#dent~_9999aFx001";
        check_eq(run_lexer(src), &[Identifier]);
    }

    #[test]
    fn string() {
        let src = r#""hello world!""#;
        check_eq(run_lexer(src), &[
            String {
                set : StrSet::Normal {
                    closed : true
                }
            }
        ]);
    }

    #[test]
    fn byte_string() {
        let src = r#"b"hello world!""#;
        check_eq(run_lexer(src), &[
            String {
                set : StrSet::ByteStr {
                    closed : true
                }
            }
        ]);
    }

    #[test]
    fn raw_byte_string() {
        let src = r##"br#"hello "\"wow"world!"#"##;
        check_eq(run_lexer(src), &[
            String {
                set : StrSet::RawByteStr {
                    hashes : 1,
                    err    : RawStrErr::NoError
                }
            }
        ]);
    }

    #[test]
    fn raw_string() {
        let src = r##"r#"hello "\"wow"world!"#"##;
        check_eq(run_lexer(src), &[
            String {
                set : StrSet::RawStr {
                    hashes : 1,
                    err    : RawStrErr::NoError
                }
            }
        ]);
    }

    #[test]
    fn char() {
        let src = "'9'";
        check_eq(run_lexer(src), &[
            Char {
                closed : true,
                set    : CharSet::Normal
            }
        ]);
    }

    #[test]
    fn byte_char() {
        let src = "b'9'";
        check_eq(run_lexer(src), &[
            Char {
                closed : true,
                set    : CharSet::ByteChar
            }
        ]);
    }

    #[test]
    fn single_line_comment() {
        let src = "// hello!";
        check_eq(run_lexer(src), &[
            SingleLineComment {
                set : CommentSet::None
            }
        ]);
    }

    #[test]
    fn multi_line_comment() {
        let src = "/* hello!\n/* world! */ */";
        check_eq(run_lexer(src), &[
            MultiLineComment {
                set    : CommentSet::None,
                closed : true,
                depth  : 0,
            }
        ]);
    }

    #[test]
    fn single_line_doc_comment() {
        let src = "/// I am a doc!";
        check_eq(run_lexer(src), &[
            SingleLineComment {
                set : CommentSet::Doc,
            }
        ]);
    }

    #[test]
    fn multi_line_doc_comment() {
        let src = "/** hello!\n/* world! /* !!! */ / */ . */";
        check_eq(run_lexer(src), &[
            MultiLineComment {
                set    : CommentSet::Doc,
                closed : true,
                depth  : 0,
            }
        ]);
    }

    #[test]
    fn invalid() {
        let src = "\\";
        check_eq(run_lexer(src), &[Invalid]);
    }
}