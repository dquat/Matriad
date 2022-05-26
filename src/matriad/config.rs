use crossterm::style::Color;

macro_rules! color {
    ($(#[doc = $doc: expr] $name: ident, $value: expr,)*) => {
        $(
            #[doc = $doc]
            pub const $name: Color = $value;
        )*
    }
}

// Color configurations for the messages
color!(
    /// The color of line numbers
    NUMBER_COLOR, Color::Cyan,
    /// The color of the wrapped lines that have no number on them, but instead have a `-`
    NO_NUMBER_COLOR, Color::Green,
    /// The color of pipes separating line numbers
    PIPE_COLOR, Color::Cyan,

    /// The `-` pointer color
    POINTER_MINUS_COLOR, Color::Red,
    /// The `+` pointer color
    POINTER_PLUS_COLOR, Color::Green,
    /// The pointer color that is for errors
    POINTER_ERROR_COLOR, Color::Red,
    /// The pointer color that is for warnings and weak warnings
    POINTER_WARN_COLOR, Color::Yellow,
    /// The pointer color that is for information messages
    POINTER_INFO_COLOR, Color::White,

    /// The color of help messages provided by the pointer
    POINTER_HELP_MSG_COLOR, Color::Green,
    /// The color of error messages provided by the pointer
    POINTER_ERROR_MSG_COLOR, Color::Red,
    /// The color of information messages provided by the pointer
    POINTER_INFO_MSG_COLOR, Color::White,
    /// The color of the text where an insert has occured
    POINTER_INSERT_COLOR, Color::Blue,

    /// The color of errored lines
    ERROR_COLOR, Color::Red,
    /// The color of lines with warnings
    WARN_COLOR, Color::Yellow,
    /// The color of lines with weak warnings
    WEAK_WARN_COLOR, Color::Yellow,
    /// The color of lines with information
    INFO_COLOR, Color::Reset,
    /// The color of empty lines
    EMPTY_COLOR, Color::Blue,
    /// The color of file names
    FILE_COLOR, Color::Magenta,

    /// The color of a visualizer label
    LABEL_COLOR, Color::White,
);
