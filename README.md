
# Alan Compiler 
[![build](https://github.com/avramidis-stavros-compilers-org/compilers-mirror/actions/workflows/rust.yml/badge.svg)](https://github.com/avramidis-stavros-compilers-org/compilers-mirror/actions/workflows/rust.yml)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)


Compiler of the Alan programming language, developed for the 8th semester's Compilers course at NTUA.


> ### Μερικές έξτρα σημειώσεις, που ελπίζω να σας γλυτώσουν χρόνο [εδώ](./docs/alan-instructions.pdf)


## Author ✍️

[Stavros Avramidis](https://www.github.com/purpl3F0x)

___
The project contains 3 subdirectories (sub-crates).
- **[alan](./alan/)**: The compiler backend for the Alan language
- **[alanc](./alanc/)**: The cli frontend of the compiler
- **[stdlib](./stdlib/)**: The Alan standard library, written in C (which at it's current state relies on libc, so we can target every platform &/ architecture)
___

## Build Instructions 🛠️
(Not as detailed as IKEA's)

Pre-build linux binaries are available, in the actions tab.

### Requirements

A pre-configured [Dockerfile](https://gist.github.com/purpl3F0x/786ceaffcd16508ae90bfa60921521b4) and an image in [docker-hub](https://hub.docker.com/r/asder/alan-image) that is (somewhat) tested can be used to easily build and test the project.

#### Rust - Cargo
The project is tested on `rustc 1.82.0-nightly`.
At the time of writing a nightly version is required.

#### LLVM - Clang
The project is requires and pre-configured to use LLVM 18.

If llvm is not in the default path `LLVM_SYS_180_PREFIX` should be set. <br>
If you want to use any other version of llvm, [Cargo.toml](./alan/Cargo.toml) of alan crate should be configured accordingly.

In addition, llvm-sys will <u>**try**</u> to link llvm statically, (platforms such Arch will be built with dynamic llvm libraries)

In sort, in order to fully build and run the compiler. The following should be available (without version prefixes or as symlinks).
- llvm-config
- llvm-as
- clang

Also (for compiling the stdlib):
- make


### Building

Clone the project

```bash
git clone https://github.com/avramidis-stavros-compilers-org/compilers-mirror.git
```

Go to the project directory

```bash
cd compilers
```

Run with cargo
```bash
cargo run alanc --release -- <Program arguments>
```

Build with cargo
```
cargo build --release
```
__Warning__: *By default x86 release builds are build for x86_64_v3 (avx,avx2) with lto*

## Install 🚀
If you want to install the *alanc* globally:
```bash
cargo install
```

## Usage 💡
Run the program with `-h/--help` and you'll learn everything you need to know, on how to use it. 

Run it with `--version`, if you want to chase some dragons. 

## Features ✨
- Cross Compilation (yes, you can compile for CUDA 🙌)

- (Ridiculously) fast lexing, using [Logos](https://github.com/maciejhirsz/logos)

- Errors made for humans, thanks to [chumsky](https://github.com/zesterer/chumsky) parser combinator + [ariadne](https://github.com/zesterer/ariadne)
    - ![alt text](./assets/error_example_parse.png)
    - ![alt text](./assets/error_example_sem1.png)
    - ![alt text](./assets/error_example_sem2.png)
    - ![alt text](./assets/error_example_sem3.png)

## Known Bugs ☠️
- [ ] Parsing lower bound negative integer (f.ex -2147483648 for 32b ints)


### ToDo
- [X] Cross compilation (partially working)

##
`
Copyright 2024 Stavros Avramidis
`

