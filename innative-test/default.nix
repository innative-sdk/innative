{ pkgs ? import <nixpkgs> { }, makeflags ? [], version ? "unknown", cppflags ? "", innative, innative-deps, innative-envs, innative-scripts, ... }:

with pkgs;
let
in
  stdenv.mkDerivation {
    name = "innative-test";
    version = version;
    src = ./.;
    LDFLAGS = "-Lbin";
    CPPFLAGS = "-I${innative}/innative -I${innative-envs}/innative-env ${cppflags}";
    INNATIVE_LIBDIR = "${innative}/lib";
    ENV_LIBDIR = "${innative-envs}/lib";
    SCRIPTS_DIR = "${innative-scripts}/scripts";
    CXXLD="$(CXX)";
    
    makeFlags = makeflags;
    buildInputs = [ innative innative-deps innative-envs innative-scripts pkgs.zlib ];
  
    installPhase = ''
    mkdir -p $out/bin/
    cp -r ./bin/* $out/bin/
    '';
  }