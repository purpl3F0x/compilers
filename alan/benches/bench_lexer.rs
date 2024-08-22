use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};

use alan::lexer::*;

static SOURCE: &str = r#"
main(): proc
    x: int;
    y: byte;
    z: int[100000];
    a: byte[100000];
    (* comment
        Three for the Kings
        Of the elves high in light
        Nine to the mortal
        Which cry
    *)
    foo(b: reference int): int
            foo(b: reference int): int
        {
            return 0;
        }
    {
        return 0;
    }
{
    -- This is a comment
    writeInteger(42);
    writeString("Hello, World!!!!!!!!!!!!!!!!!!!!");
    x = 42;    
    x = 42;
    x = 42;
    x = 42;
    x = 42;
    x = 42;
    x = 42;
    x = 42;
    x = 42;
    x = 42;
    x = 42;
    x = 42;

    z[99] = -2 * -1;
    z[100] = 2 + 3 * 3 * ( 2 / 3 + 7);

    if (z[99] > z[100]) {
        x = 42;
    } else {
        x = 42;
    }

}
"#;

#[allow(unused_must_use)]
fn iterate(s: &str) {
    use logos::Logos;

    let mut lex = Token::lexer(s);

    while let Some(token) = lex.next() {
        black_box(token);
    }
}

fn benchmark_lexer(c: &mut Criterion) {
    let mut group = c.benchmark_group("iterate");
    group.throughput(Throughput::Bytes(SOURCE.len() as u64));
    group.bench_with_input("program", &SOURCE, |b, &s| b.iter(|| iterate(s)));
}

criterion_group!(benches, benchmark_lexer);
criterion_main!(benches);
