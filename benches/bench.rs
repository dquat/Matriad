use criterion::{black_box, criterion_group, criterion_main, Criterion};

#[path = "../src/language/lexer.rs"]
mod lexer;

pub fn chars(c: &mut Criterion) {
    // test out the speed of the lexer
    // currently standing at about 2.600us(microseconds, 1,000,000th of a second) per iteration,
    // and this is 415 chars in length. = ~160MB/s of data lexed on an i7-6500U
    c.bench_function("chars", |b| {
        let src = r#"// a random set of random code (comment)
        /* nother comment */
        fn main() { \nlet x = 1; x += 10 ** 2; let str = "les_go man! \x1B[35;5;9myo!\x1B[0m"; }
        if x > 10.000 && y > 20.38827 + 2873 ** 298'3' { if (66 + y * 32.02883, 8382, 3984) == (3 * 9.308, 3) { return "bOOm!"; }; };
        let IamAnIdentifierAndThisIsALongIdentifierThatShouldBeASingleToken = 3.322222'1' + "\x7F OOh \u7721 boy!""#;
        b.iter(move || {
            let mut lexer = black_box(lexer::Lexer::new(black_box(src)));
            while !lexer.eof() {
                black_box(lexer.next());
            }
        });
    });
}

criterion_group!(benches, chars);
criterion_main!(benches);