{
  description = "Rust dev shell with LLVM and dependencies";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            llvm
            libxml2
            libffi
            ncurses
          ];

          shellHook = ''
            LD_LIBRARY_PATH=${pkgs.llvm.lib}/lib:${pkgs.stdenv.cc.cc.lib}/lib:${pkgs.libffi}/lib:${pkgs.ncurses}/lib:$LD_LIBRARY_PATH
          '';
        };
      }
    );
}
