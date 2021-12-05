pub mod lexer;

fn main() {
    let srcs = [
        "** *= **= & && &= | || |= < << > >>",
    ];
    for src in srcs {
        let mut lexer = lexer::Lexer::new(src);
        while !lexer.eof() {
            println!("{:?}", lexer.next());
        }
        println!("\n");
    }
}
