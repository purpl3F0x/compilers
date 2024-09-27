# The Debian Cookbook

## Requirements Instaltion
(Tested in debian-12-wsl and Debian 12.5)

### Install requirements
```sh
sudo apt install build-essential wget curl gnupg lsb-release software-properties-common libzstd-dev zlib1g-dev
```

### Install rustup, rust, cargo
This will install the 1.82-nightly rust build and set it as a default toolchain.
```sh
curl https://sh.rustup.rs -sSf | sh -s -- -y --default-toolchain nightly
```
Reload the environment
```sh
. "$HOME/.cargo/env"
```

\* The rust compiler <u>should only be installed and managed by rustup</u>, the apt version and any other package manager's rustc binary are not meant for development.

### Install LLVM-18
(if llvm apt-keys are already installed, `sudo apt install clang-18 llvm-18 libpolly-18-dev`)
```
curl -s https://apt.llvm.org/llvm.sh | sudo bash -s 18 all
```

### Verify the Installations
```sh
llvm-config-18 --version && rustc --version && cargo --version
```


## Compile the project
### Build
```sh
cargo build --release
```
### Test
```sh
cargo test --all
```
### Get distribution files
```sh
python ./make_dist.py ?[An optional folder path]
```

(you can always manually copy the `[alanc, libalan.a]` files form `/target/release/`

All working ??
```sh
alanc --version
```

## Building Docs
```sh
cargo doc --all --document-private-items
```
