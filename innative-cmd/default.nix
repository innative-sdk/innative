{ pkgs ? import <nixpkgs> { }, makeflags ? [], version ? "unknown", cppflags ? "", innative, innative-deps, ... }:

with pkgs;
let
in
  stdenv.mkDerivation {
    name = "innative-cmd";
    version = version;
    src = ./.;
    LDFLAGS = "-Lbin";
    CPPFLAGS = "-I${innative}/innative ${cppflags}";
    INNATIVE_LIBDIR = "${innative}/lib";
    CXXLD="$(CXX)";
    
    makeFlags = makeflags;
    buildInputs = [ innative innative-deps pkgs.zlib ];
  
    installPhase = ''
    mkdir -p $out/bin/
    cp -r ./bin/* $out/bin/
    '';
  }