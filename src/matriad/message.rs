// ! This is now somewhat usable, probably buggy and feature incomplete

// I have no idea how this works, or what this is. If anybody asks, I did not make it.
// The galactic powers of the universe forced my fingers to crunch on my small laptop and generate
// this galactic mess.

// If you faced problems after reading this code, I am deeply sorry.

// imports
use std::{
    borrow::Cow,
    fmt::{
        Display,
        Formatter,
    },
    io::{
        Write,
        stdout,
    },
};

use crossterm::style::Stylize;
use unicode_width::{
    UnicodeWidthChar,
    UnicodeWidthStr,
};

use crate::matriad::{
    util::Span,
    config::*,
};

use crossterm::style::Color;

/// A structure to hold pointer data, including where it should be located and
/// what kind of pointer character should be used
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Pointer<'a> {
    /// The pointer character. Usually something like `~`, `^`, `-`, `+`
    pub pointer  : char,
    /// The span of the pointer, the start to end locations
    pub span     : Span,
    /// The message that is "attached" to the pointer, if any
    pub message  : Option<&'a str>,
    /// The value that this pointer inserts, if any
    pub insert   : Option<(usize, &'a str)>,
    /// The colors of the pointer. The first being the color of the `pointer` and the second
    /// being the color of the message, if any
    pub colors   : (Color, Option<Color>),
}

impl<'a> Pointer<'a> {
    /// Create a new pointer
    pub fn new(pointer: char, span: Span) -> Self {
        Self {
            message : None,
            insert  : None,
            colors  : (POINTER_ERROR_COLOR, None),
            pointer,
            span,
        }
    }

    /// Set the message of the pointer
    pub fn set_msg(&mut self, message: &'a str, color: Color) -> &mut Self {
        self.message  = Some(message);
        self.colors.1 = Some(color);
        self
    }

    /// Set the color of the `pointer` field
    pub fn set_pointer_color(&mut self, color: Color) -> &mut Self {
        self.colors.0 = color;
        self
    }

    /// Set the insert string of the pointer
    pub fn set_insert(&mut self, location: usize, value: &'a str) -> &mut Self {
        self.insert = Some((location, value));
        self
    }
}

/// A structure for visualizing the message range, based off of the source code.<br/>
/// This is used for displaying error, warning and information messages.<br/>
/// ## Example Output<br/>
/// <pre>
///  - |
///  1 |    let message =;
///  - |    ^^^          ^
///  - |    |            and another message here
///  - |    a message here
/// </pre>
#[derive(Debug, PartialEq)]
pub struct Visualizer<'a> {
    /// The source code
    pub src      : Cow<'a, str>,
    /// The lines to show
    pub lines    : Span,
    /// The start location of the message (used to find out where the pointers start)
    pub start    : usize,
    /// The name of this visualizer
    pub label    : &'a str,
    /// The lines that are wrapped, and should be excluded from counting
    pub exclude  : Vec<usize>,
    /// The pointers that show where the message is referring to in the source code
    pub pointers : Vec<Pointer<'a>>,
    /// The type of the visualizer. If none, defaults to the [`MsgType::Error`] variant
    pub msg_type : Option<MsgType>,
}

impl<'a> Clone for Visualizer<'a> {
    fn clone(&self) -> Self {
        Self {
            // Only clone some parts, better performance... maybe?
            src      : self.src     .clone(),
            exclude  : self.exclude .clone(),
            pointers : self.pointers.clone(),
            ..*self
        }
    }
}

impl<'a> Visualizer<'a> {
    /// Create a new visualizer
    pub fn new(
        src        : &'a str,
        line_start : usize,
        line_end   : usize,
        label      : &'a str,
    ) -> Self {
        let mut lines = Span::new(line_start, line_end);
        // Swap if end is greater than start
        if lines.start > lines.end { lines.swap(); }
        // Find the length of the source till the starting line
        let start =
            src
                .lines()
                .take(lines.start)
                .collect::<String>()
                .chars()
                .count();
        Self {
            src      : Cow::Borrowed(src),
            exclude  : Vec::new(),
            pointers : Vec::new(),
            msg_type : None,
            lines,
            start,
            label,
        }
    }

    /// Create a new visualizer from a [`Span`]
    pub fn new_span(
        src   : &'a str,
        mut lines : Span,
        label : &'a str,
    ) -> Self {
        // Swap if end is greater than start
        if lines.start > lines.end { lines.swap(); }
        // Find the length of the source till the starting line
        let start =
            src
                .lines()
                .take(lines.start)
                .collect::<String>()
                .chars()
                .count();
        Self {
            src      : Cow::Borrowed(src),
            exclude  : Vec::new(),
            pointers : Vec::new(),
            msg_type : None,
            lines,
            start,
            label,
        }
    }

    /// Add a pointer to the pointer vector
    #[inline]
    pub fn add_ptr(&mut self, pointer: Pointer<'a>) -> &mut Self {
        self.pointers.push(pointer);
        // Sort the pointers based on their start location, in ascending order
        self.pointers.sort_by(|a, b| a.span.start.cmp(&b.span.start));
        self
    }

    /// Set the start location of the visualizer
    #[inline]
    pub fn set_start(&mut self, start: usize) -> &mut Self {
        self.start = start;
        self
    }

    /// Checks if the pointer starts on a specific line
    #[inline]
    fn starts_on_line(pointer: &Pointer, start: usize, end: usize) -> bool {
        pointer.span.start >= start && pointer.span.start < end
    }

    /// Checks if a pointer covers all of the specific line
    #[inline]
    fn is_on_line(pointer: &Pointer, start: usize, end: usize) -> bool {
        pointer.span.start <= start && pointer.span.end >= end
    }

    /// Checks if a pointer ends on the specific line
    #[inline]
    fn end_on_line(pointer: &Pointer, start: usize, end: usize) -> bool {
        pointer.span.end >= start && pointer.span.end < end
    }

    #[inline]
    fn point(pointers: &mut [&Pointer], max_len: usize, pos: usize) {
        for i in 0..pointers.len() {
            // The padding to be applied to the left
            print!("{}", &" ".repeat(max_len));
            print!("{}", " | ".with(PIPE_COLOR));
            // These are the pointers we are currently considering
            let pointers = &pointers[0..(pointers.len() - i)];
            // This stores the previous start location of the previous span
            let mut prev = 0;
            for i in 0..pointers.len() {
                // The start location of this span
                let start = pointers[i].span.start - pos + 1;
                // We have encountered the same position
                if start == prev {
                    // We're at the end and we have encountered the same position as before
                    // Print this message too
                    if i + 1 == pointers.len() {
                        print!("{} {}", ">".with(PIPE_COLOR), pointers[i].message.unwrap());
                    }
                    // Skip the iteration, since we don't need to add all the other characters
                    continue;
                }
                // Add required spaces
                print!("{}", &" ".repeat(pointers[i].span.start - pos - prev));
                // If we're at the end, print the message
                if i + 1 == pointers.len() {
                    print!("{}",
                           pointers[i]
                               .message
                               .unwrap()
                               .with(
                                   pointers[i]
                                       .colors.1
                                       .unwrap()
                               )
                    );
                }
                // If not, we print an extender (`|`)
                else { print!("{}", "|".with(PIPE_COLOR)); }
                // Store the previously accessed location
                prev = start;
            }
            // Print a newline
            println!();
        }
    }

    pub fn show(&mut self) {
        // The lines that incorporate this visualizer
        // The wrap function is expected to be called first, before this
        let lines = self.src.lines();
        // The number of excluded lines we have currently encountered
        let mut excluded = 0usize;
        // The maximum line number length that this visualizer has, as a string
        let max_len = self.lines.end.to_string().len();
        let mut pos = self.start + self.lines.start;
        // Loop through lines
        for (line, val) in lines.enumerate() {
            // TODO: use unicode width here? Probably. Will introduce a lot of work I assume
            let val_len = val.chars().count();
            let mut end_pos = pos + val_len;
            let is_excluded = self.exclude.contains(&line);
            let is_empty = val.is_empty();
            // Show that the text in that line is empty
            let mut val = Cow::Borrowed(val);
            // Holds the line number
            let mut num = Cow::Borrowed("-");
            // Check if line is excluded, i.e. wrapped by the visualizer
            if is_excluded {
                // This line is excluded, so increment the excluded counter
                excluded += 1;
            } else {
                // Increment the end position to account for the extra new line
                end_pos += 1;
                // Convert relative lines to absolute lines
                let line = line + self.lines.start + 1;
                // This line is not excluded, so populate the line number
                num = Cow::Owned((line - excluded).to_string());
            };
            // The string that all the pointers get added to
            let mut pointer_string = String::new();
            let mut pointer_len = 0;
            // The previous relative end location of a pointer
            let mut prev = 0;
            // if this line is excluded, account for an extra newline
            // i.e. consider:
            // 1. abcdefg\n
            // -  hijklmnop
            // 2. qrstuvwxy\n
            // The fist line's newline will be accounted for when we
            // reach the wrapped line, and move the location of the
            // pointer. To prevent this, we do this check to see if the
            // current line is wrapped, and account for the miscalculation
            let mut newline_pad = if is_excluded { 1 } else { 0 };
            // Loop through all pointers in the current line
            for pointer in self.pointers.iter() {
                // This stores the calculated length of pointers to display on this current line,
                // for this specific pointer
                let mut calculated_length = 0;
                if Self::starts_on_line(pointer, pos, end_pos) {
                    // Get the starting location of the pointer
                    // This is calculated relative to the current line being considered
                    let start = pointer.span.start - pos;
                    // Get the length of the pointer
                    let mut len = pointer.span.len();
                    // If the starting location is more than the previous location, there are no
                    // overlaps, so print spaces based on the distance covered previously
                    if start >= prev {
                        let start = start + newline_pad;
                        pointer_string += &" ".repeat(start - prev);
                        pointer_len += start - prev;
                    }
                    // If the length is more than the difference of the previous location and the
                    // previous location is greater than the start, i.e. the start intersects the end
                    // of the previous pointer, reduce the length by the difference
                    else if len > prev - start {
                        len -= prev - start;
                    }
                    // If the pointer completely gets covered by the previous pointer, then don't print
                    // it by setting the length to 0
                    else {
                        len = 0;
                    }
                    calculated_length +=
                        if len + pointer_len > val_len {
                            val_len - pointer_len
                        } else {
                            len
                        };
                } else if Self::is_on_line(pointer, pos, end_pos) {
                    calculated_length +=
                        // If we have already completely covered the area, then don't
                        // add any more pointers
                        if prev + 1 >= val_len && prev != 0 {
                            0
                        }
                        // If it has not completely been covered, then we can push how many
                        // pointers we need to complete the line
                        else {
                            val_len - prev
                        };
                } else if Self::end_on_line(pointer, pos, end_pos) {
                    let diff = pointer.span.end - pos;
                    calculated_length +=
                        // If the difference is greater than the previous. This needs
                        // to be checked since we don't order the pointers based on their
                        // end locations, but their start locations
                        if diff > prev ||
                            // This checks if we are at the start of the line and it is
                            // an excluded line. If so, we basically get one pointer
                            (diff == prev && diff == 0)
                        {
                            diff - prev + newline_pad
                        } else {
                            0
                        };
                } else {
                    // Nothing on this line. Continue as normal
                    continue;
                }
                if let Some((loc, str)) = pointer.insert {
                    let string = format!("{}", str.with(POINTER_INSERT_COLOR));
                    Self::insert(&mut val, loc - pos, &string);
                    pointer_string +=
                        &pointer
                            .pointer
                            .to_string()
                            .repeat(str.len())
                            .with(pointer.colors.0)
                            .to_string();
                    pointer_len += str.len();
                }
                // Add the pointer to the string based on the length calculated above
                pointer_string +=
                    &pointer
                        .pointer
                        .to_string()
                        .repeat(calculated_length)
                        .with(pointer.colors.0)
                        .to_string();
                pointer_len += calculated_length;
                // Store the previous ending location of the pointer, for the next iteration
                prev = pointer.span.end - pos;
                // Remove the newline pad since it would be accounted for in the previous iteration?
                newline_pad = 0;
            }
            // Look for pointers with messages that start on this line
            let iter =
                self.pointers
                    .iter()
                    .filter(|p|
                        Self::starts_on_line(p, pos, end_pos) &&
                            p.message.is_some()
                    );
            // Generate a slice from the above
            let mut slice = Vec::from_iter(iter);
            let mut slice = slice.as_mut_slice();
            print!(
                "{}{} {} {}\n",
                " ".repeat(max_len - num.len()),
                num.to_string().with(NUMBER_COLOR),
                "|".with(PIPE_COLOR),
                if is_empty {
                    "<EMPTY>".to_owned().with(EMPTY_COLOR)
                } else {
                    val.to_string().with(match self.msg_type {
                        Some(MsgType::Information)  => INFO_COLOR,
                        Some(MsgType::Warning)      => WARN_COLOR,
                        Some(MsgType::WeakWarning)  => WEAK_WARN_COLOR,
                        Some(MsgType::Error) | None => ERROR_COLOR,
                    })
                },
            );
            if !pointer_string.is_empty() {
                // format the pointer, since it's not empty
                print!(
                    "{} {} {pointer_string}\n",
                    " ".repeat(max_len),
                    "|".with(PIPE_COLOR),
                )
            };
            stdout().flush().ok();
            // Create helper messages from the above slices
            Self::point(
                slice,
                max_len,
                // Account for the extra newline on wrapped lines
                pos - if is_excluded { 1 } else { 0 }
            );
            pos = end_pos;
        }
    }

    pub fn wrap(
        &mut self,
        max_width   : usize,
        mut padding : usize
    ) -> &mut Self {
        // if padding is more than max width, subtracting will cause an overflow, so we use
        // max width without any padding, by setting padding to 0
        if padding > max_width { padding = 0; }
        // add a bit of padding if needed to get the actual width
        let width = max_width - padding;
        // holds the newly generated lines
        let mut new_lines =
            Vec::with_capacity(
                self.src
                    .matches('\n')
                    .count()
            );
        // The lines that incorporate this visualizer
        let lines =
            self.src
                .lines()
                // Skip the lines from the start
                .skip(self.lines.start)
                // Take all the lines selected
                .take(self.lines.len() + 1);
        if max_width != 0 {
            // Offset index (moves when newline characters are added)
            let mut offset = 0;
            for (i, line) in lines.enumerate() {
                let len = UnicodeWidthStr::width(line);
                if len <= width {
                    // If line does not need to be wrapped, continue
                    new_lines.push(line.to_owned());
                } else {
                    // If it does need to be wrapped, split it up, and join it with a newline
                    let mut new_line = String::with_capacity(len + len / width);
                    // current character skip count. Used because the `take` functions moves the
                    // iterator, requiring it to be re-generated every time, lowering performance
                    let mut current = 0;
                    // used to skip the first line to exclude from numbering
                    let mut un_number = false;
                    loop {
                        // get the string slice that is <= width
                        let mut push_str =
                            line
                                .chars()
                                .skip(current)
                                .take(width)
                                .collect::<String>();
                        // if the string is empty, i.e. the line has ended, break out of the loop
                        if push_str.is_empty() { break; }
                        // add the width to the current skip count for the next iteration
                        current += width;
                        // exclude the line from numbering when displaying when un_number is true,
                        // i.e. we are not at the first line to wrap
                        if un_number { self.exclude.push(offset + i); }
                        offset += 1; // line number push offset
                        // add the newline and push the string to the main line we're modifying
                        push_str.push('\n');
                        new_line.push_str(push_str.as_str());
                        // first line is over, set un_number to true to exclude from numbering
                        un_number = true;
                    }
                    // Remove extra newlines that were added when wrapping. The wrapper does not check
                    // weather it's at the end of the source or not
                    new_line.pop();
                    // Reduce the offset, since the newline was removed
                    offset -= 1;
                    // Push the new line to the lines we're generating
                    new_lines.push(new_line);
                }
            }
            // This fixes a bug where when the lines end on an empty line
            new_lines.push("".to_owned());
            let mut src = new_lines.join("\n");
            src.pop();
            // assign the values that were computed
            self.src = Cow::Owned(src);
        } else {
            self.src =
                Cow::Owned(
                    lines
                        .collect::<Vec<&str>>()
                        .join("\n")
                );
        }
        self
    }

    /// Insert a value into a [`Cow`] string
    fn insert(value: &mut Cow<'_, str>, location: usize, str: &str) {
        match value {
            Cow::Owned(ref mut string) => string.insert_str(location, str),
            Cow::Borrowed(string) => {
                let mut owned = string.to_owned();
                owned.insert_str(location, str);
                *value = Cow::Owned(owned);
            },
        };
    }
}

/// The message type of the [`Msg`] structure
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MsgType {
    /// Messages that show information to the user. Not sure what this could be used for
    Information,
    /// A warning about something that does not impact readability and would not point out potential bugs
    WeakWarning,
    /// A warning that impacts readability and could also be pointing out potential bugs
    Warning,
    /// A syntax error, type error, parse error etc. This will most probably mean that the compiler
    /// has not produced an executable / byte-code (in later stages of this language)
    Error,
}

impl Default for MsgType {
    fn default() -> Self { Self::Error }
}

impl Display for MsgType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            MsgType::Information => "Info",
            MsgType::WeakWarning => "Weak Warn",
            MsgType::Warning     => "Warn",
            MsgType::Error       => "Error",
        };
        write!(f, "{}", s)
    }
}

/// The message struct to display messages to the user
#[derive(Debug, PartialEq)]
pub struct Msg<'a, S0: Clone + Display, S1: Clone + Display> {
    /// The actual message to display to the user
    pub message      : S0,
    /// The file that this message's source file corresponds to
    pub file         : S1,
    /// The source code that this message points to
    pub src          : &'a str,
    /// The range of the message
    pub span         : Span,
    /// The type of message (see [`MsgType`] for all available types)
    pub msg_type     : MsgType,
    /// The code of the message (can be empty)
    pub msg_code     : Option<&'a str>,
    /// The visualizers, that show where the message points to
    pub visualizers  : Vec<Visualizer<'a>>,
}

impl<'a, S0: Clone + Display, S1: Clone + Display> Clone for Msg<'a, S0, S1> {
    fn clone(&self) -> Self {
        Self {
            message     : self.message    .clone(),
            file        : self.file       .clone(),
            src         : self.src        .clone(),
            visualizers : self.visualizers.clone(),
            ..*self
        }
    }
}

impl<'a, S0: Clone + Display, S1: Clone + Display> Msg<'a, S0, S1> {
    /// Create a new message
    pub fn new(
        message    : S0,
        file       : S1,
        src        : &'a str,
        line_start : usize,
        line_end   : usize,
        msg_type   : MsgType,
    ) -> Self {
        Self {
            msg_type,
            message,
            file,
            src,
            span         : Span::new(line_start, line_end),
            visualizers  : Vec::new(),
            msg_code     : None,
        }
    }

    /// Create a new message from a span
    pub fn new_span(
        message  : S0,
        file     : S1,
        src      : &'a str,
        span     : Span,
        msg_type : MsgType,
    ) -> Self {
        Self {
            msg_type,
            message,
            file,
            span,
            src,
            visualizers  : Vec::new(),
            msg_code     : None,
        }
    }

    /// Set the message code
    pub fn set_code(&mut self, code: &'a str) -> &mut Self {
        self.msg_code = Some(code);
        self
    }

    /// Add a visualizer to the visualizer array
    pub fn add_vis(&mut self) -> &mut Visualizer<'a> {
        let mut vis = Visualizer::new_span(self.src, self.span, "");
        match crossterm::terminal::size() {
            Ok((width, _)) => {
                vis.wrap(width as usize, 0);
            },
            _ => {
                vis.wrap(0, 0);
            }
        };
        vis.msg_type = Some(self.msg_type);
        self.visualizers.push(vis);
        self.visualizers.last_mut().unwrap()
    }

    /// Generates the messages and shows it
    pub fn show(&mut self) {
        // Select color
        let color = match self.msg_type {
            MsgType::Information => INFO_COLOR,
            MsgType::Warning     => WARN_COLOR,
            MsgType::WeakWarning => WEAK_WARN_COLOR,
            MsgType::Error       => ERROR_COLOR,
        };

        let code =
            match self.msg_code {
                Some(code) => Cow::Owned(format!(" [{}]", code.with(color))),
                _ => Cow::Borrowed(""),
            };

        println!(
            "{}{}: {}\n  --> {}",
            self.msg_type.to_string().with(color),
            code,
            self.message.to_string().with(color),
            self.file,
        );

        for vis in self.visualizers.iter_mut() {
            vis.show();
        }
    }
}