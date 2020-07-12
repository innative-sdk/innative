{ pkgs ? import <nixpkgs> { }, makeflags ? [], version ? "unknown", cppflags ? "", innative-envs, innative-deps, ... }:

with pkgs;
let
  llvm = callPackage ../llvm.nix {};
in
  stdenv.mkDerivation {
    name = "inNative";
    version = version;
    src = ./.;
    LDFLAGS = "-Lbin";
    CPPFLAGS = "${cppflags}";
    CXXLD="$(CXX)";
    
    ENV_LIBDIR = "${innative-envs}/lib";
    
    makeFlags = makeflags;
    buildInputs = [ innative-envs innative-deps llvm ];
    ARCHIVE_PREFIX = "${llvm}/lib/";
  
    installPhase = ''
    mkdir -p $out/lib/
    cp -r ./lib/* $out/lib/
    mkdir -p $out/innative/
    cp -r ./*.h $out/innative/
    '';
  }