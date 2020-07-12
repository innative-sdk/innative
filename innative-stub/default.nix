{ pkgs ? import <nixpkgs> { }, makeflags ? [], version ? "unknown", cppflags ? "", innative-deps, ... }:

with pkgs;

let
in
  stdenv.mkDerivation {
    name = "innative-stub";
    version = version;
    src = ./.;
    
    CPPFLAGS = "${cppflags}";
    
    buildInputs = [ innative-deps ];
    makeFlags = makeflags;
    installPhase = ''
    mkdir -p $out/lib
    cp -r ./lib/* $out/lib/
    '';
  }