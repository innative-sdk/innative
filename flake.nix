{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
    nix-filter.url = "github:numtide/nix-filter";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    { self, nixpkgs, nix-filter, flake-utils }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });

    in {
      packages = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
          selfpkgs = self.packages.${system};
          llvmpkgs = pkgs.llvmPackages_13.tools.extend (_: _: { enableSharedLibraries = false; });
        in {
          innative = pkgs.stdenv.mkDerivation {
            pname = "innative";
            version = "0.2.0";
            src = nix-filter.lib.filter {
              root = ./.;
              include = [
                "CMakeLists.txt"
                (nix-filter.lib.inDirectory "innative")
                (nix-filter.lib.inDirectory "innative-assemblyscript")
                (nix-filter.lib.inDirectory "innative-cmd")
                (nix-filter.lib.inDirectory "innative-env")
                (nix-filter.lib.inDirectory "innative-loader")
                (nix-filter.lib.inDirectory "innative-stub")
                (nix-filter.lib.inDirectory "innative-test")
                (nix-filter.lib.inDirectory "innative-test-embedding")
                (nix-filter.lib.inDirectory "include")
                (nix-filter.lib.inDirectory "scripts")
                (nix-filter.lib.inDirectory "spec")
              ];
            };
            enableParallelBuilding = true;

            nativeBuildInputs = [ pkgs.cmake ];
            buildInputs = [
              llvmpkgs.libclang
              llvmpkgs.llvm.dev
              llvmpkgs.lld
              pkgs.libxml2
            ];

            cmakeFlags = [ "-DUSE_DEFAULT_FOLDERS=1" "-DUSE_BUILD_OVERRIDE=1"];

            outputs = [ "out" ];
          };
        }
      );
      
      defaultPackage = forAllSystems (system: self.packages.${system}.innative);
      devShell = forAllSystems (system: self.packages.${system}.innative);
    };
}