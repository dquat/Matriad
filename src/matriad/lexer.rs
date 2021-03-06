// Several things were borrowed from the Rustc lexer

use std::str::Chars;

use crate::matriad::{
    token::{
        lexer::{
            Set::*,
            *,
        },
        *,
        Delimiters::*,
    },
    util::Span,
};

type OutToken = Token<Set>;

/// The Lexer that lexes a given source and generates a token based off of the source
/// every time it's [Lexer::next](`next`) function is called
#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    /// The current position of the lexer, relative to the length of the source
    pub pos  : usize,
    /// The current line of the lexer, relative to the number of lines present in the source
    pub line : usize,
    /// The [`Chars`] iterator generated from the source that is used to generate and identify
    /// the type of token that the lexer will output in the [Lexer::next](`next`) function
    pub it   : Chars<'a>,
}

impl<'a> Lexer<'a> {
    /// Creates a new lexer
    #[inline]
    pub fn new(src: &'a str) -> Self {
        Self {
            pos  : 0,
            line : 0,
            it   : src.chars(),
        }
    }

    // utility functions
    /// Takes characters off the [`Chars`] iterator until the predicate returns false
    #[inline]
    fn take_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        loop {
            // Clones the iterator then assigns the value to the iterator if the predicate accepts.
            // Doing this is faster than calling peek, then advance,
            // since we call the `next` function twice, unnecessarily in that scenario

            // This same concept is used much throughout this lexer, but peek has also been used
            // in cases where I felt the difference would be much too negligible
            let mut clone = self.it.clone();
            match clone.next() {
                Some(c) if predicate(c) => {
                    self.it = clone;
                    if c == '\n' {
                        self.line += 1;
                    }
                    self.pos += c.len_utf8();
                },
                _ => break,
            };
        }
    }

    /// Used when we know that no newlines and no multi-symbol characters will be found
    #[inline]
    fn take_while_unchecked(&mut self, mut predicate: impl FnMut(char) -> bool) {
        loop {
            let mut clone = self.it.clone();
            match clone.next() {
                Some(c) if predicate(c) => {
                    self.it = clone;
                    self.pos += 1;
                },
                _ => break,
            };
        }
    }

    /// Advances the [`Chars`] iterator
    #[inline]
    fn adv(&mut self) {
        let next =
            match self.it.next() {
                Some(v) => v,
                None => return,
            };
        if next == '\n' {
            self.line += 1;
        }
        self.pos += next.len_utf8();
    }

    /// Advances the [`Chars`] iterator without performing checks
    /// Used whenever we know there are no newlines and the character is only 1 symbol long
    #[inline]
    fn adv_unchecked(&mut self) {
        match self.it.next() {
            Some(_) => self.pos += 1,
            None => return,
        };
    }

    /// Peeks the iterator once
    #[inline]
    fn peek(&self) -> char { self.it.clone().next().unwrap_or('\0') }

    /// Checks if the [`Lexer`] has reached the End Of File (EOF) yet, or not
    pub fn eof(&self) -> bool { self.it.clone().next().is_none() }

    /// Creates a token from two characters if the check matched,
    /// or else just uses the current character and returns the token
    #[inline]
    fn double(&mut self, no_matched: Delimiters, matched: Delimiters, check: char) -> Option<OutToken> {
        let (line, start) = (self.line, self.pos);
        self.adv_unchecked();
        let mut delimiter = no_matched;
        let mut clone = self.it.clone();
        if clone.next() == Some(check) {
            self.it = clone;
            self.pos += 1;
            delimiter = matched;
        }
        Some(OutToken::new(
            Delimiter(delimiter),
            Span::new(start, self.pos),
            Span::new(line, self.line)
        ))
    }

    /// Same as [above](Lexer::double) but performs 3 checks, `char check`, `char =`
    /// and if none of them pass, just the `char`
    #[inline]
    fn triple(
        &mut self,
        check      : char,
        no_matched : Delimiters, // char
        matched1   : Delimiters, // char check
        matched2   : Delimiters, // char =
    ) -> Option<OutToken> {
        let (line, start) = (self.line, self.pos);
        self.adv_unchecked();
        // let peek = self.peek();
        let mut clone = self.it.clone();
        let mut delimiter = no_matched;
        match clone.next() {
            Some(c) if c == check => {
                self.it = clone;
                self.pos += 1;
                delimiter = matched1;
            },

            Some('=') => {
                self.it = clone;
                self.pos += 1;
                delimiter = matched2;
            },

            _ => (),
        }
        Some(OutToken::new(
            Delimiter(delimiter),
            Span::new(start, self.pos),
            Span::new(line, self.line)
        ))
    }

    /// Same as [above](Lexer::triple) but adds one more check: `char check =`
    #[inline]
    fn quadruple(
        &mut self,
        check      : char,
        no_matched : Delimiters, // char
        matched1   : Delimiters, // char check
        matched2   : Delimiters, // char =
        matched3   : Delimiters, // char check =
    ) -> Option<OutToken> {
        let (line, start) = (self.line, self.pos);
        self.adv_unchecked();
        let peek = self.peek();
        let mut delimiter = no_matched;
        if peek == check {
            self.adv_unchecked();
            let peek = self.peek();
            if peek == '=' {
                self.adv_unchecked();
                delimiter = matched3;
            } else {
                delimiter = matched1;
            }
        } else if peek == '=' {
            self.adv_unchecked();
            delimiter = matched2;
        }
        Some(OutToken::new(
            Delimiter(delimiter),
            Span::new(start, self.pos),
            Span::new(line, self.line)
        ))
    }

    // Used by the next function
    /// Checks for the start of an identifier
    #[inline]
    fn ident_start(c: char) -> bool {
        matches! (
            c,
            'a'..='z' |
            'A'..='Z' |
            '_'
        )
    }

    /// Checks if the identifier can "continue" being an identifier at that character
    #[inline]
    fn ident_continue(c: char) -> bool {
        // Some of these symbols may be removed later as the language progresses,
        // and cannot accommodate the symbols to be present in identifiers
        matches! (
            c,
            'a'..='z' |
            'A'..='Z' |
            '0'..='9' |
            '_'       |
            '~'       |
            '#'
        )
    }

    /// Takes a number (integer) where `_` and `0..9` are valid symbols
    #[inline]
    fn number(&mut self) -> bool {
        let mut empty = true;
        // This can be unchecked since we're not dealing with newlines or multi-symbol characters
        self.take_while_unchecked(|c| {
            match c {
                '_' => (),
                '0'..='9' => { empty = false; },
                _ => return false
            };
            true
        });
        empty
    }

    /// Gets all the characters between two of the same symbols,
    /// used for lexing characters & strings
    #[inline]
    fn between(&mut self, char_or_str: char) -> bool {
        // Take the starting character
        self.adv_unchecked();
        // Previous character to check if any escape sequence was constructed including the check
        // character. If so, it must be skipped
        let mut prev = '\0';
        self.take_while(|c| {
            let res = c != char_or_str || prev == '\\';
            prev = c;
            res
        });
        // Check if the item was really closed
        let mut clone = self.it.clone();
        if clone.next() == Some(char_or_str) {
            // We know that the character is a single symbol as it's only used for parsing
            // strings and characters in this lexer
            self.pos += 1;
            self.it = clone;
            true
        } else {
            false
        }
    }

    /// Gets all the characters between `'` and `'`. This is meant to be only one symbol long,
    /// but that won't be checked here as we could have multiple characters between a
    /// Char type when using escape sequences, which are not parsed in this scope
    #[inline]
    fn char(&mut self, set: CharSet) -> Option<OutToken> {
        let (line, start) = (self.line, self.pos);
        let closed = self.between('\'');
        Some(OutToken::new(
            Char { closed, set },
            Span::new(start, self.pos),
            Span::new(line, self.line)
        ))
    }

    /// Lexes an identifier and returns the respective token
    #[inline]
    fn ident(&mut self) -> Option<OutToken> {
        let (line, start) = (self.line, self.pos);
        // allows identifiers such as `_919#`, `_~#.`, `ident`, `__~_.` etc
        self.take_while_unchecked(Self::ident_continue);
        Some(OutToken::new(
            Identifier,
            Span::new(start, self.pos),
            Span::new(line, self.line)
        ))
    }

    /// Lexes a raw string and checks for any errors
    #[inline]
    fn raw_str(&mut self) -> (usize, RawStrErr) {
        let start = self.pos;
        // take the starting hashes
        self.take_while_unchecked(|c| c == '#');
        let start_hashes = self.pos - start;
        let next_char = self.peek();
        // did not expect a random character between `r#` and `"`
        if next_char != '\"' {
            let pos = self.pos;
            self.adv();
            return (
                start_hashes,
                RawStrErr::UnexpectedChar {
                    unexpected : next_char,
                    location   : pos,
                });
        }
        self.adv_unchecked();
        let mut end_hashes = 0;
        let (mut max_hashes, mut prev_pos) = (0, self.pos);

        // Loop through the iterator till we find the ending hashes, or encounter an error
        loop {
            // Take the string between the hashes
            self.take_while(|c| c != '\"');
            // We found the end of file before the hashes were matched!
            if self.eof() {
                return (start_hashes, RawStrErr::LacksHashes {
                    lacking : start_hashes - end_hashes,
                    hint    : (max_hashes, prev_pos)
                })
            }
            // Consume the closing quote
            self.adv_unchecked();
            let end_start = self.pos;
            // Take the ending hashes
            loop {
                let mut clone = self.it.clone();
                match clone.next() {
                    Some('#') if (self.pos - end_start) <= start_hashes => {
                        // We know that hashes cannot be more than one symbol long,
                        // and they are most certainly not new-line characters
                        self.it = clone;
                        self.pos += 1;
                    },
                    _ => break,
                };
            }
            end_hashes = self.pos - end_start;
            // If the start hashes equals the end hashes, the string has been closed,
            // so we can now safely return it
            if start_hashes == end_hashes {
                return (start_hashes, RawStrErr::NoError);
            }
            else if end_hashes > max_hashes {
                max_hashes = end_hashes;
                prev_pos = end_start;
            }
            // if not, continue looping till we find an error or the string closes
        }
    }

    #[inline]
    fn take_suffix(&mut self, c: char) {
        if Self::ident_start(c) {
            self.take_while_unchecked(Self::ident_continue);
        }
    }

    // The main function of this lexer
    /// Generates the next token from it's [`Chars`] iterator and returns it as an [`Option`]
    #[inline]
    pub fn next(&mut self) -> Option<OutToken> {
        // These two values get used quite frequently (mostly in the macro now)
        let (line, start) = (self.line, self.pos);
        // A simple macro to reduce repetitive code
        macro_rules! tok {
            ($token: expr) => {
                Some(Token::new(
                    $token,
                    Span::new(start, self.pos),
                    Span::new(line, self.line)
                ))
            }
        }

        macro_rules! delimiter {
            ($del: ident) => {{
                self.adv_unchecked();
                Some(Token::new(
                    Delimiter($del),
                    Span::new(start, self.pos),
                    Span::new(line, self.line)
                ))
            }};
        }

        // Used to peek, twice, where needed
        let mut clone = self.it.clone();
        // Lexes the next token based on the general number of the appearence of such symbols in code
        match clone.next()? {
            // Lex whitespace
            // char::is_whitespace is significantly faster than the previous method for whatever
            // reason
            c if char::is_whitespace(c) => {
                self.take_while(char::is_whitespace);
                tok!(Whitespace)
            },

            // These delimiters occur quite a lot in most code, so they are checked for first
            ')' => delimiter!(LeftParen),
            '(' => delimiter!(RightParen),

            '}' => delimiter!(LeftBrace),
            '{' => delimiter!(RightBrace),

            ';' => delimiter!(Semicolon),

            // numbers and identifiers are also significant parts of code
            // Get numbers (hex, octal, binary, decimal or exponent)
            c @ '0'..='9' => {
                let mut set = NumberSet::Normal;
                // Consume the digit
                self.adv_unchecked();
                if c == '0' {
                    let empty = match self.peek() {
                        // Found a binary number
                        'b' => {
                            self.adv_unchecked();
                            set = NumberSet::Binary;
                            self.number()
                        }

                        // Found an octal number
                        'o' => {
                            self.adv_unchecked();
                            set = NumberSet::Octal;
                            self.number()
                        }

                        // Found a hexadecimal number
                        'x' => {
                            self.adv_unchecked();
                            set = NumberSet::Hex;
                            let mut empty = true;
                            self.take_while_unchecked(|c| {
                                match c {
                                    '_' => (),
                                    '0'..='9' | 'a'..='f' | 'A'..='F' => {
                                        empty = false;
                                    },
                                    _ => return false
                                };
                                true
                            });
                            empty
                        }

                        // This is just a normal integer
                        '0'..='9' | '_' => {
                            self.take_while(|c| matches!(c, '_' | '0'..='9'));
                            false
                        }

                        // Just a normal `0` integer
                        _ => {
                            // Take the suffix, if any
                            let suffix_start = self.pos;
                            self.take_suffix(self.peek());
                            return tok!(Int { set, empty: false, suffix_start });
                        }
                    };

                    // An empty number was found: `0x` | `0b` | `0o`
                    if empty {
                        // Take the suffix, if any
                        let suffix_start = self.pos;
                        self.take_suffix(self.peek());
                        return tok!(Int { set, empty, suffix_start })
                    }
                } else {
                    // Parse as a normal integer
                    self.take_while_unchecked(|c| matches!(c, '_' | '0'..='9'));
                }
                let mut clone = self.it.clone();
                match (clone.next(), clone.next()) {
                    // A decimal place was found along with a number alongside it
                    // `1.` will not mach this criteria and will be lexed as `1` and a Dot
                    // `1.5` will be successfully lexed as a floating point number: `1.5`
                    (Some('.'), Some('0'..='9')) => {
                        // Consume the `.` symbol
                        self.adv_unchecked();
                        let mut empty = false;
                        // Lex the number to the right of the `.` symbol
                        self.number();
                        let (num_end, mut exp_start) = (self.pos, self.pos);
                        match self.peek() {
                            // Found a floating point exponent to parse: `1.5e+10` | `1.9e11` etc.
                            'e' | 'E' => {
                                // Same thing is done below
                                self.adv_unchecked();
                                exp_start = self.pos;
                                if matches!(self.peek(), '-' | '+') {
                                    self.adv_unchecked();
                                }
                                // The exponent had no number to the right of the exponent
                                // if the `empty` boolean is true
                                empty = self.number();
                            }
                            _ => ()
                        }
                        // Take the suffix, if any
                        let suffix_start = self.pos;
                        self.take_suffix(self.peek());
                        tok!(Float {
                            set,
                            empty,
                            suffix_start,
                            number   : Span::new(start, num_end),
                            exponent : Span::new(exp_start, suffix_start)
                        })
                    }

                    // Parse an exponent when no decimal place was found: `2e+10` | `3e-10` | `4e10`
                    (Some('e' | 'E'), _) => {
                        let num_end = self.pos;
                        // Consume the `E` or `e`
                        self.adv_unchecked();
                        // Consume the `+` or `-` symbols (only if they exist, of course)
                        let exp_start = self.pos;
                        if matches!(self.peek(), '-' | '+') {
                            self.adv_unchecked();
                        }
                        // Consume the exponent number
                        let empty = self.number();
                        // Take the suffix, if any
                        let suffix_start = self.pos;
                        self.take_suffix(self.peek());
                        tok!(Float {
                            set,
                            empty,
                            suffix_start,
                            number   : Span::new(start, num_end),
                            exponent : Span::new(exp_start, suffix_start)
                        })
                    }

                    // A normal integer was found; There was no decimal place
                    _ => {
                        // Take the suffix, if any
                        let suffix_start = self.pos;
                        self.take_suffix(self.peek());
                        tok!(Int { set, empty: false, suffix_start })
                    }
                }
            },

            // Byte strings / chars and raw strings are even more rare than normal ones
            // but they must be put before identifiers, or else they will never be parsed

            // Check for raw strings
            'r' => match clone.next() {
                // Found a raw string
                Some('#' | '\"') => {
                    // advance to consume the `r`
                    self.adv_unchecked();
                    let (hashes, mut err) = self.raw_str();
                    // Try to convert hash number to a u8.
                    // If it errors, we found too many hashes!
                    match u8::try_from(hashes) {
                        Ok(_) => (),
                        _ => err = RawStrErr::ExcessHashes,
                    };
                    tok!(String { set: StrSet::RawStr { hashes: hashes as u8, err } })
                },

                // Continue as a normal identifier
                _ => self.ident(),
            }

            // Check for byte characters, byte strings, or raw byte strings
            'b' => match clone.next() {
                // This is a byte character
                Some('\'') => {
                    // Consume the `b` character
                    self.adv_unchecked();
                    self.char(CharSet::ByteChar)
                },

                // This is a byte string
                Some('\"') => {
                    // Consume the `b` character
                    self.adv_unchecked();
                    let closed = self.between('\"');
                    tok!(String { set: StrSet::ByteStr { closed } })
                }

                // Found a raw byte string
                Some('r') if matches!(clone.next(), Some('#' | '\"')) => {
                    // Advance twice to consume `br`
                    self.adv_unchecked();
                    self.adv_unchecked();
                    let (hashes, mut err) = self.raw_str();
                    // Try to convert hash number to a u8.
                    // If it errors, we found too many hashes!
                    match u8::try_from(hashes) {
                        Ok(_) => (),
                        _ => err = RawStrErr::ExcessHashes,
                    };
                    tok!(String { set: StrSet::RawByteStr { hashes: hashes as u8, err } })
                }

                // Continue as a normal identifier
                _ => self.ident(),
            }

            // Get identifiers & keywords
            c if Self::ident_start(c) => self.ident(),

            // These are the significantly less used operators and delimiters
            // so they are lower than other delimiters / operators
            ']' => delimiter!(LeftBracket),
            '[' => delimiter!(RightBracket),

            '.' => self.double(Dot, Range, '.'), // . | ..
            ':' => self.double(Colon, DoubleColon, ':'), // : | ::
            '+' => self.double(Plus, PlusEqual, '='), // + | +=
            '-' => self.double(Minus, MinusEqual, '='), // - | -=
            '=' => self.double(Assign, Equal, '='), // = | ==
            '!' => self.double(Not, NotEqual, '='), // ! |  !=

            // Comments and slashes usually consist of around 1/10 of most "proper" code
            // This is a wild guess of course
            '/' => {
                // Consume the slash
                self.adv_unchecked();
                match self.peek() {
                    // Single line comment
                    // /// Doc comment
                    // // Normal comment
                    '/' => {
                        // Consume the second slash
                        self.adv_unchecked();
                        let mut clone = self.it.clone();
                        // Check if a third slash occurs. If so, it's a doc comment
                        // If not, or a 4th slash occurs, it is not a doc comment
                        let set = match clone.next() {
                            Some('/') if clone.next() != Some('/') => CommentSet::Doc,
                            _ => CommentSet::None,
                        };
                        self.take_while(|c| c != '\n');
                        // Consume newline, or do nothing if eof
                        self.adv();
                        tok!(SingleLineComment { set })
                    }

                    // Assuming that divide equal is more prominent than a multiline comment
                    // but less prominent than a single line comment
                    // /=
                    '=' => {
                        self.adv_unchecked();
                        tok!(Delimiter(DivideEqual))
                    },

                    // Multiline comment
                    // /** Doc comment */
                    // /* Normal */
                    '*' => {
                        self.adv_unchecked();
                        let mut clone = self.it.clone();
                        let set = match clone.next() {
                            Some('*') if !matches!(clone.next(), Some('*' | '/')) =>
                                CommentSet::Doc,
                            _ => CommentSet::None,
                        };
                        // Parses multiline comments, layer by layer. Allows nested comments
                        // Invalid comment: `/* /* */` (Nesting broken)
                        let mut layer = 1usize;
                        while let Some(c) = self.it.next() {
                            self.pos += c.len_utf8();
                            if c == '\n' { self.line += 1; }
                            // We found a starting comment symbol, so increment nested layer
                            let peek = self.peek();
                            if c == '/' && peek == '*' {
                                layer += 1;
                                // Consume the `*` symbol
                                self.adv_unchecked();
                            } else if c == '*' && peek == '/' {
                                // Consume the `/` symbol
                                self.adv_unchecked();
                                // We found a closing comment symbol, so decrement the nested layer
                                layer -= 1;
                                // If there are no layers that are incomplete, i.e. the layer is 0,
                                // stop parsing the comment as we have reached the end
                                if layer == 0 {
                                    break;
                                }
                            }
                        };
                        tok!(MultiLineComment { set, closed: layer == 0, depth: layer })
                    }

                    // /
                    _ => tok!(Delimiter(Divide)),
                }
            }

            // Strings and characters are usually scarce in normal code, with only a few lines
            // containing a significant amount so these are low in the checks

            // This is a normal string
            '\"' => {
                let closed = self.between('\"');
                tok!(String { set: StrSet::Normal { closed } })
            },

            // This is a normal character
            '\'' => self.char(CharSet::Normal),

            // These are quite scarce operators, but can be found in abundance in if statements
            // and such other statements. I'm not sure weather this one should go higher or not

            '&' =>
                self.triple(
                    '&',
                    BitAnd,
                    And,
                    BitAndEqual
                ), // & | &= | &&

            '|' =>
                self.triple(
                    '|',
                    BitOr,
                    Or,
                    BitOrEqual,
                ), // | or |= or ||

            '<' =>
                self.quadruple(
                    '<',
                    Lesser,
                    BitLeftShift,
                    LesserEqual,
                    BitLeftShiftEqual
                ), // < | <= | << | <<=

            '>' =>
                self.quadruple(
                    '>',
                    Greater,
                    BitRightShift,
                    GreaterEqual,
                    BitRightShiftEqual
                ), // > | >= | >> | >>=

            '*' =>
                self.quadruple(
                    '*',
                    Multiply,
                    Exponent,
                    MultiplyEqual,
                    ExponentEqual
                ), // * | ** | *= | **=

            // These operators are hardly found in code, so they are very low in prominence

            '%' => self.double(Modulo, ModuloEqual, '='), // % | %=
            '^' => self.double(BitXor, BitXorEqual, '='), // ^ | ^=

            // These are pretty obvious in what they do, don't you think?
            '@' => delimiter!(At),
            '#' => delimiter!(Hash),
            '~' => delimiter!(Tilde),
            ',' => delimiter!(Comma),
            '$' => delimiter!(Dollar),
            '`' => delimiter!(BackTick),
            '?' => delimiter!(Question),

            // Well, one hardly finds invalid characters in code unless it was intentionally
            // put there so pretty obviously it is the lowest.
            // Even if invalid characters existed in abundance, this one cannot be moved up since
            // it can match any character, so anything below it won't run at all

            // An invalid character was found. Will later be converted to an error
            _ => {
                self.adv();
                tok!(Invalid)
            }
        }
    }
}