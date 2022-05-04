// ! Buggy, and incomplete
// imports
use std::borrow::{Cow};
use std::fmt::{Display, Formatter};
use crossterm::style::Stylize;
use crate::matriad::util::Span;

/// A structure to hold pointer data, including where it should be located and
/// what kind of pointer character should be used
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Pointer<'a> {
    pub pointer  : char,
    pub span     : Span,
    pub message  : Option<&'a str>,
}

/// A structure for visualizing the message range, based off of the source code.<br/>
/// This is used for displaying error, warning and information messages.<br/>
/// src [`Cow<'a, str>`](Cow): The source code of the message<br/>
/// range [`Cow<'a, str>`](Cow): The source, containing only the specified lines of the message<br/>
/// span [`Span`]: The range of the message<br/>
/// lines [`Span`]: The line range of the message (how many lines this message spans)<br/>
/// label `&'a str`: The name this visualizer carries<br/>
/// Implements [`Clone`], [`Display`], [`Debug`], [`Default`], [`Eq`], [`PartialEq`], [`Hash`]<br/>
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
    pub src      : Cow<'a, str>,
    pub span     : Span,
    pub lines    : Span,
    pub label    : &'a str,
    pub exclude  : Vec<usize>,
    pub pointers : Vec<Pointer<'a>>,
}

impl<'a> Clone for Visualizer<'a> {
    fn clone(&self) -> Self {
        Self {
            // Only clone some parts, better performance
            src      : self.src     .clone(),
            exclude  : self.exclude .clone(),
            pointers : self.pointers.clone(),
            ..*self
        }
    }
}

impl<'a> Visualizer<'a> {
    pub fn new(
        src        : &'a str,
        start      : usize,
        end        : usize,
        line_start : usize,
        line_end   : usize,
        label      : &'a str,
    ) -> Self {
        let mut span  = Span::new(start, end);
        let mut lines = Span::new(line_start, line_end);
        // swap if end is greater than start
        if span.start > span.end   { span .swap(); }
        if lines.start > lines.end { lines.swap(); }
        Self {
            src      : Cow::Borrowed(src),
            exclude  : Vec::new(),
            pointers : Vec::new(),
            lines,
            span,
            label,
        }
    }

    pub fn new_span(
        src       : &'a str,
        mut span  : Span,
        mut lines : Span,
        label     : &'a str,
    ) -> Self {
        // swap if end is greater than start
        if span.start > span.end   { span .swap(); }
        if lines.start > lines.end { lines.swap(); }
        Self {
            src      : Cow::Borrowed(src),
            exclude  : Vec::new(),
            pointers : Vec::new(),
            lines,
            span,
            label,
        }
    }

    pub fn show(&self) -> String {
        // create string to store the new message
        let mut string = String::with_capacity(self.span.len());
        let mut add_exclude = 0usize;
        let max_len = self.lines.end.to_string().len();
        // loop through lines
        for (line, val) in self.src.lines().enumerate() {
            if line < self.lines.start { continue; }
            if line > self.lines.end { break; }
            let val_checked =
                if val.trim().is_empty() { "<empty>".bold().blue() }
                else                     { val             .red () };
            if self.exclude.contains(&line) {
                let str = format!("{}- | {}\n", " ".repeat(max_len - 1), val_checked);
                let str = str.as_str();
                string.push_str(str);
                add_exclude += 1;
            } else {
                let num = line - add_exclude;
                let num_len = num.to_string().len();
                let str =
                    format!("{}{} | {}\n", " ".repeat(max_len - num_len), num, val_checked);
                let str = str.as_str();
                string.push_str(str);
            }
        }
        string
    }

    pub fn compute_whitespace(&mut self) -> &mut Self {
        let (start, end) = (self.span.start, self.span.end);
        // if the span is empty or there are no tabs, do nothing
        if end == start || self.src.matches('\t').count() == 0 { return self; }
        // calculate number of tabs from starting of source, to starting of message and compensate
        let start_disp =
            &self.src
                .chars()
                .take(start)
                .filter(|c| *c == '\t')
                .count() * 3;
        // calculate number of tabs from starting of message, to ending of message and compensate
        let end_disp   =
            &self.src
                .chars()
                .skip(self.span.start)
                .take(self.span.len())
                .filter(|c| *c == '\t')
                .count() * 3;
        // push the message span, to compensate converting the tabs to spaces
        self.span.start += start_disp;
        self.span.end   += start_disp + end_disp;
        self.src         = Cow::Owned(self.src.replace('\t', "    "));
        self                                                       //  ^^^^ <- 4 spaces = 1 tab
    }

    pub fn wrap(
        &mut self,
        max_width   : usize,
        mut padding : usize
    ) -> &mut Self {
        let (mut start, mut end) = (self.span.start, self.span.end);
        // if padding is more than max width, subtracting will cause an overflow, so we use
        // max width without any padding, by setting padding to 0
        if padding > max_width { padding = 0; }
        // add a bit of padding if needed to get the actual width
        let width = max_width - padding;
        // holds the newly generated lines
        let mut new_lines = Vec::with_capacity(
            self.src
                    .matches('\n')
                    .count()
        );
        // stores the current character index. used to push the start and end locations if
        // the line is too long, and we add a newline
        let mut curr_i = 0;
        let mut offset = 0; // offset index (moves when newline characters are added)
        for (i, line) in self.src.lines().enumerate() {
            let len = line.len();
            if len <= width {
                // if line does not need to be wrapped, continue
                curr_i += line.chars().count();
                new_lines.push(line.to_string());
            } else {
                // if it does, split it up, and join it with a newline
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
                    // push the start and end locations by 1 to compensate for the newline to add
                    match current + curr_i {
                        c if c <= start             => { start += 1; end += 1; },
                        c if c >  start && c <= end =>               end += 1,
                        _                           => ()
                    }
                    // add the newline and push the string to the main line we're modifying
                    push_str.push('\n');
                    new_line.push_str(push_str.as_str());
                    // first line is over, set un_number to true to exclude from numbering
                    un_number = true;
                }
                // push the new line to the lines we're generating
                new_lines.push(new_line);
            }
        };
        // when the wrapper adds an extra newline to the source, remove it. This can occur
        // because it does not check if it's at the end of the source.
        let mut src = new_lines.join("\n");
        if !self.src.ends_with('\n') && src.ends_with('\n') {
            src.pop();
        }
        // assign the values that were computed
        self.span.start = start;
        self.span.end   = end;
        self.src        = Cow::Owned(src);
        self
    }
}

impl<'a> Display for Visualizer<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,
               "{}{}{}",
               self.label,
               if !self.label.is_empty() { ":\n" } else { "" },
               self.show()
        )
    }
}

/// The message type of the [`Msg`] structure.<br/>
/// The available types are: <br/>
/// `MsgType::Error`<br/> `MsgType::Warning`<br/>
/// `MsgType::WeakWarning`<br/> `MsgType::Information`<br/>
/// Implements [`Clone`], [`Copy`], [`Debug`], [`Display`],
/// [`Default`], [`Eq`], [`PartialEq`], [`Hash`]<br/>
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MsgType {
    Information,
    WeakWarning,
    Warning,
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

/// The message struct to display messages to the user<br/>
/// src [`Cow<'a, str>`](Cow): The source code that this message points to<br/>
/// span [`Span`]: The range of the message<br/>
/// msg_type [`MsgType`]: The type of message (see [`MsgType`] for all available types)<br/>
/// msg_code [`Option<&'a str>`](Option): The code of the message (this can be empty)<br/>
/// message `S0:` [`Clone`] + [`Display`]: The actual message to display to the user<br/>
/// file `S1:` [`Clone`] + [`Display`]: The file that this message's source file corresponds to<br/>
/// Implements [`Debug`], [`Display`], [`Default`], [`Clone`], [`Eq`], [`PartialEq`], [`Hash`]<br/>
#[derive(Debug, PartialEq)]
pub struct Msg<'a, S0: Clone + Display, S1: Clone + Display> {
    pub message  : S0,
    pub file     : S1,
    pub src      : Cow<'a, str>,
    pub span     : Span,
    pub msg_type : MsgType,
    pub msg_code : Option<&'a str>,
    pub main_vis : Option<Visualizer<'a>>,
}

impl<'a, S0: Clone + Display, S1: Clone + Display> Clone for Msg<'a, S0, S1> {
    fn clone(&self) -> Self {
        Self {
            message  : self.message.clone(),
            file     : self.file.clone(),
            src      : self.src.clone(),
            main_vis : self.main_vis.clone(),
            ..*self
        }
    }
}

impl<'a, S0: Clone + Display, S1: Clone + Display> Msg<'a, S0, S1> {
    pub fn new(
        message  : S0,
        file     : S1,
        src      : Cow<'a, str>,
        start    : usize,
        end      : usize,
        msg_type : MsgType,
    ) -> Self {
        Self {
            message,
            file,
            src,
            msg_type,
            span     : Span::new(start, end),
            main_vis : None,
            msg_code : None,
        }
    }

    pub fn new_span(
        message  : S0,
        file     : S1,
        src      : Cow<'a, str>,
        span     : Span,
        msg_type : MsgType,
    ) -> Self {
        Self {
            message,
            file,
            src,
            span,
            msg_type,
            main_vis : None,
            msg_code : None,
        }
    }

    // set message code, if any
    pub fn set_code(&mut self, code: &'a str) -> &mut Self {
        self.msg_code = Some(code);
        self
    }

    // set main visualizer, if any
    pub fn set_main_vis(&mut self, vis: Visualizer<'a>) -> &mut Self {
        self.main_vis = Some(vis);
        self
    }
}

impl<S0: Display + Clone, S1: Display + Clone> Display for Msg<'_, S0, S1> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let code =
            match self.msg_code {
                Some(code) => format!(" [{}]", code),
                _          => String::new(),
            };
        let main_vis =
            match &self.main_vis {
                Some(vis) => format!("{}", vis),
                _         => String::new(),
            };
        write!(
            f,
            "{}{}: {}\n  --> {} (from location {} to {})\n{}",
            self.msg_type,
            code,
            self.message,
            self.file,
            self.span.start,
            self.span.end,
            main_vis,
        )
    }
}