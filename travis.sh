#!/bin/bash

set -e
set -x

if [[ $CHECK_CLANG_FORMAT -eq 1 ]]; then
    if [[ $(uname) = Linux ]]; then
        wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | sudo apt-key add -
        sudo add-apt-repository -y "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-9 main"
        for i in {1..5}; do sudo apt-get update -qq && break || sleep 15; done
        sudo apt-get install -y clang-format-9
        export PATH="/usr/lib/llvm-9/bin:$PATH"
    else
        exit 1
    fi
    which clang-format

    ./format.sh
    git status
    git diff
    git diff-index --quiet HEAD
    exit 0
fi

if [[ $(uname) = Linux ]]; then
  sudo apt-get update -qq
  mkdir bin/
  wget https://github.com/innative-sdk/llvm-project/releases/download/v9.0.0-innative/lld-9.0.0-x86-64-linux-ubuntu-18.tar.gz
  wget https://github.com/innative-sdk/llvm-project/releases/download/v9.0.0-innative/llvm-9.0.0-x86-64-linux-ubuntu-18.tar.gz
  tar -C bin/ -zxf lld-9.0.0-x86-64-linux-ubuntu-18.tar.gz
  tar -C bin/ -zxf llvm-9.0.0-x86-64-linux-ubuntu-18.tar.gz
  pushd bin
  mv llvm-9.0.0-x86-64-posix llvm
  mv lld-9.0.0-x86-64-posix/include/lld llvm/include/lld
  mv lld-9.0.0-x86-64-posix/bin/* llvm/bin
  mv lld-9.0.0-x86-64-posix/lib/* llvm/lib
  popd

  make
  pushd bin
  ./innative-test
  popd
fi
