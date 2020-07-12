{ pkgs ? import <nixpkgs> { }, makeflags ? [], version ? "unknown", cppflags ? "", innative-deps, ... }:

with pkgs;

let
in
  stdenv.mkDerivation {
    name = "innative-env";
    version = version;
    src = ./.;
    
    CPPFLAGS = "${cppflags}";
    INNATIVE_ENV_SRC = ".";
    
    buildInputs = [ innative-deps ];
    makeFlags = makeflags;
    installPhase = ''
    mkdir -p $out/lib
    cp -r ./lib/* $out/lib/
    mkdir -p $out/innative-env/
    cp -r ./*.h $out/innative-env/
    '';
  }