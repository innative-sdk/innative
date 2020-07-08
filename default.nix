{ pkgs ? import <nixpkgs> {} }:

with pkgs;
let
  llvm = callPackage ./llvm.nix {};
in
  stdenv.mkDerivation {
    pname = "innative";
    version = "0.1.8.0";
    src = ./.;
    
    buildInputs = [ llvm ];
    ARCHIVE_PREFIX = "${llvm}/lib/";
  }