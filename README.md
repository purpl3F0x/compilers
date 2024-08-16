
# Alan Compiler [![build](https://github.com/purpl3F0x/compilers/actions/workflows/rust.yml/badge.svg?branch=main)](https://github.com/purpl3F0x/compilers/actions/workflows/rust.yml)

Semester project for 2024 Comilers' class.

### Author

- [Stavros Avramidis](https://www.github.com/purpl3F0x)

___
The project contains 3 subdirectories (subcrates).
- **[alan](./alan/)**: The compiler backend for the Alan language
- **[alanc](./alanc/)**: The cli frontend of the compiler
- **[stdlib](./stdlib/)**: The Alan standard library, writen in C (which at it's current state relies on libc, so we can target every plattform &/ architecture)
___

### Run Locally

Clone the project

```bash
git clone https://github.com/purpl3F0x/alanc.git
```

Go to the project directory

```bash
cd alanc-master
```

Run with cargo
```bash
cargo run alanc --release  -- <Program arguments>
```

Build with cargo
```
cargo build --release
```

__Warning: By default release builds are build for x86_64_v3 (avx,avx2) with lto__


##
`
Copyright 2024 Stavros Avramidis
`

