{ pkgs ? import <nixpkgs> { } }:

with pkgs;
let
  llvm = callPackage ./llvm.nix {};
in
  mkShell {
    buildInputs = [ llvm ];
    ARCHIVE_PREFIX = "${llvm}/lib/";
  }
