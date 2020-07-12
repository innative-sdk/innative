{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;

let
  version = "0.1.8";
  debugflags = "-DNDEBUG -O3";
  rawflags = "-Wall -Wshadow -Wno-attributes -Wno-unknown-pragmas -Wno-missing-braces -Wno-unused-function -Wno-comment -Wno-char-subscripts -Wno-sign-compare -Wno-unused-variable -Wno-switch";
  cppflags = "${rawflags} ${debugflags}";
  makeflags = [ "BINDIR=bin" "LIBDIR=lib" "OBJDIR=bin/obj" ];
  innative-deps = stdenv.mkDerivation {
      name = "innative-deps";
      version = version;
      src = ./include;
      installPhase = ''
      mkdir -p $out/include/
      cp -r . $out/include/'';
    };
  innative-scripts = stdenv.mkDerivation {
      name = "innative-scripts";
      version = version;
      src = ./scripts;
      installPhase = ''
      mkdir -p $out/scripts/
      cp -r . $out/scripts/'';
    };
in
  rec {
    innative-envs = callPackage ./innative-env { cppflags=rawflags; inherit version makeflags innative-deps; };
    innative = callPackage ./innative { inherit version cppflags makeflags innative-deps innative-envs; };
    innative-cmd = callPackage ./innative-cmd { inherit version cppflags makeflags innative-deps innative; };
    innative-test-embedding = callPackage ./innative-test-embedding { inherit version cppflags makeflags innative-deps; };
    innative-stub = callPackage ./innative-stub { inherit version cppflags makeflags innative-deps; };
    innative-test = callPackage ./innative-test { inherit version cppflags makeflags innative innative-deps innative-envs innative-scripts; };
  }