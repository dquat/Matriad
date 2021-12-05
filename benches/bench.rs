use criterion::{black_box, criterion_group, criterion_main, Criterion};

#[path = "../src/lexer.rs"]
mod lexer;

pub fn chars(c: &mut Criterion) {
    c.bench_function("manual", |b| {
        let srcs = [
            "fn main() { \nlet x = 1; x += 10 ** 2; }",
            "if x > 10.000 && y > 20.38827 + 2873 ** 2983 { if (66 + y * 32.02883, 8382, 3984) == (3 * 9.308, 3) {}; };",
            "let IamAnIdentifierAndThisIsALongIdentifierThatShouldBeASingleToken = 3.3222221",
        ];
        b.iter(move || {
            for src in srcs {
                let mut lexer = black_box(
                    lexer::Lexer::new(black_box(src))
                );
                while !lexer.eof() {
                    black_box(lexer.next());
                }
            }
        });
    });
}

criterion_group!(benches, chars);
criterion_main!(benches);