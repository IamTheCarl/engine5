#!/bin/bash -e

target () {
    TARGET=$1

    rm -rf target/publish/${TARGET}
    mkdir -p target/publish/${TARGET}

    cargo build --target=x86_64-unknown-linux-gnu --release --no-default-features
    cp target/${TARGET}/release/engine5 target/publish/${TARGET}
    cp -r assets target/publish/${TARGET}
}

target x86_64-unknown-linux-gnu
