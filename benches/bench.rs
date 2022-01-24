use criterion::{black_box, criterion_group, criterion_main, Criterion};

#[path = "../src/language/lexer.rs"]
mod lexer;

#[path = "../src/language/util.rs"]
mod util;
use util::SArr;

pub fn chars(c: &mut Criterion) {
    // c.bench_function("extend", |b| {
    //     b.iter(|| {
    //         let mut arr = black_box(SArr::<usize, 10>::new());
    //         arr.extend_slice_clone(&[1usize, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
    //         black_box(arr.clear());
    //     });
    // });

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