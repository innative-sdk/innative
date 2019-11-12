#!/bin/bash

set -e

for f in innative/*.h innative/*.cpp innative-cmd/*.cpp include/innative/*.h innative-env/*.c innative-loader/*.c innative-stub/*.c innative-test/*.h innative-test/*.cpp innative-test-embedding/*.c; do
    clang-format -i "$f"
done
