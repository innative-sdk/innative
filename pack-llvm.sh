#!/bin/sh
PICK=lld
if [ $# -eq 0 ]; then
  PICK=llvm
fi
TARGET=$PICK-9.0.0-x86-64-posix

mkdir -p $TARGET/bin
mkdir -p $TARGET/include/$PICK
mkdir -p $TARGET/lib
if [ "$PICK" = "llvm" ]; then
cp bin/$PICK/lib/libLLVM*.a $TARGET/lib -f
else
cp bin/$PICK/lib/liblld*.a $TARGET/lib -f
fi

cp bin/$PICK/bin/* $TARGET/bin -f
cd bin/llvm/include/$PICK/
find ./ -type f \( -iname \*.h -o -iname \*.inc -o -iname \*.def -o -iname \*.td -o -iname \*.modulemap \) -exec cp --parents {} ../../../../$TARGET/include/$PICK/ -n \;
cd ../../../../
if [ "$PICK" = "llvm" ]; then
mkdir -p $TARGET/include/llvm-c/
cd bin/llvm/include/llvm-c/
find ./ -type f \( -iname \*.h -o -iname \*.inc -o -iname \*.def -o -iname \*.td -o -iname \*.modulemap \) -exec cp --parents {} ../../../../$TARGET/include/llvm-c/ -n \;
cd ../../../../
fi

tar -czf $TARGET.tar.gz $TARGET/
rm -r $TARGET/
