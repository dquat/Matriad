![Matriad Logo](https://raw.githubusercontent.com/dquat/Matriad/main/matriad_name_dark.svg#gh-light-mode-only)
![Matriad Logo](https://raw.githubusercontent.com/dquat/Matriad/main/matriad_name_light.svg#gh-dark-mode-only)
# The Matriad programming language

# Prerequisites
- [Rust](https://www.rust-lang.org/) must be installed, version 1.60.0 or greater
  - Older versions of Rust may compile this code, but there are no guarantees, so it is recommended you use the latest version of Rust when compiling this program.
  - Installing cargo for [rust](https://www.rust-lang.org/) is also recommended
  - You can install both cargo and the rust compiler with [rustup](https://rustup.rs/)
- [Git](https://git-scm.com/downloads) must be installed if you want to clone the source code 
# Cloning
Clone the source-code using git like so:
```bash
# git must be installed!
git clone https://github.com/dquat/Matriad.git
```
# Execution
- This language is still not complete to the point where it's usable, but I've implemented the lexer for the most part, and you can try it out! I assume you want to look at the interesting errors more than the output though, which is very boring to look at.

```bash
# Format for running the code to check your file
# Run it through cargo:
cargo run --release <source_file> | <source_file.mrd>

# Or build the executable and use it:
cargo build --release
cd ./target/release
# Run the file
./matriad <source_file> | <source_file.mrd>
```

# Aim
This language aims to be a language that is:
- Easy to learn and use
- Performant and efficient
- Almost, or equally powerful as Rust or C++
- Fun to read and write 
  - syntax sugar to help make code shorter, and easier to read
- Feature complete 
  - almost everything needed is implemented in the standard library
- Versatile 
  - A general purpose language
  - Object-oriented or partially functional programming both have good support

This compiler aims to be:
- Simple to set up
- Easy to use
- Good at showing error / warnings and other messages
  - Compiler suggests fixes for common mistakes 
- Relatively fast at compiling source code
- Hassle-free (no bugs)
- Versatile
  - interpreted and compiled mode
# Notes
This language is currently in a:
- Stage of development where nothing is really certain
- Non-usable, but compilable state
