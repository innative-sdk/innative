#!/bin/sh
TARGET=llvm-10.0.0-x86-64-posix
SRC=bin/llvm

if [ "$1" != "" ]; then
  SRC=$1
fi

mkdir -p "$TARGET/bin"
mkdir -p "$TARGET/include/llvm"
mkdir -p "$TARGET/include/llvm-c"
mkdir -p "$TARGET/include/lld"
mkdir -p "$TARGET/lib"
cp $SRC/lib/libLLVM*.a $TARGET/lib -f
cp $SRC/lib/liblld*.a $TARGET/lib -f
cp $SRC/lib/libPolly*.a $TARGET/lib -f

OLD=`pwd`

cp $SRC/bin/* $TARGET/bin -f
cd "$SRC/include/llvm/"
find ./ -type f \( -iname \*.h -o -iname \*.inc -o -iname \*.def -o -iname \*.td -o -iname \*.modulemap \) -exec cp --parents {} "$OLD/$TARGET/include/llvm/" -n \;
cd "$OLD"

cd "$SRC/include/llvm-c/"
find ./ -type f \( -iname \*.h -o -iname \*.inc -o -iname \*.def -o -iname \*.td -o -iname \*.modulemap \) -exec cp --parents {} "$OLD/$TARGET/include/llvm-c/" -n \;
cd "$OLD"

cd "$SRC/include/lld/"
find ./ -type f \( -iname \*.h -o -iname \*.inc -o -iname \*.def -o -iname \*.td -o -iname \*.modulemap \) -exec cp --parents {} "$OLD/$TARGET/include/lld/" -n \;
cd "$OLD"


tar -czf $TARGET.tar.gz $TARGET/
rm -rf $TARGET/
