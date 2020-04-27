#!/bin/sh
TARGET=llvm-10.0.0-x86-64-posix

mkdir -p $TARGET/bin
mkdir -p $TARGET/include/llvm
mkdir -p $TARGET/lib
cp bin/llvm/lib/libLLVM*.a $TARGET/lib -f
cp bin/llvm/lib/liblld*.a $TARGET/lib -f

cp bin/llvm/bin/* $TARGET/bin -f
cd bin/llvm/include/llvm/
find ./ -type f \( -iname \*.h -o -iname \*.inc -o -iname \*.def -o -iname \*.td -o -iname \*.modulemap \) -exec cp --parents {} ../../../../$TARGET/include/llvm/ -n \;
cd ../../../../

mkdir -p $TARGET/include/llvm-c/
cd bin/llvm/include/llvm-c/
find ./ -type f \( -iname \*.h -o -iname \*.inc -o -iname \*.def -o -iname \*.td -o -iname \*.modulemap \) -exec cp --parents {} ../../../../$TARGET/include/llvm-c/ -n \;
cd ../../../../

cd bin/llvm/include/lld/
find ./ -type f \( -iname \*.h -o -iname \*.inc -o -iname \*.def -o -iname \*.td -o -iname \*.modulemap \) -exec cp --parents {} ../../../../$TARGET/include/lld/ -n \;
cd ../../../../

tar -czf $TARGET.tar.gz $TARGET/
rm -r $TARGET/
