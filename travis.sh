#!/bin/bash

set -e
set -x

if [[ $(uname) = Linux ]]; then
  sudo apt-get update -qq
  sudo apt-get install zlib1g-dev
  mkdir bin/
  wget https://github.com/innative-sdk/llvm-project/releases/download/v10.0.0-innative/llvm-10.0.0-x86-64-posix.tar.gz
  tar -C bin/ -zxf llvm-10.0.0-x86-64-posix.tar.gz
  pushd bin
  mv llvm-10.0.0-x86-64-posix llvm
  popd

  make
  pushd bin
  ./innative-test -ignore 16
  popd
fi
