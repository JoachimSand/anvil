
let
  pkgs = import <nixpkgs> { };
in
  with pkgs;
  stdenv.mkDerivation {
    name = "anvil-environment";
    buildInputs = [
      libllvm
      libz
      zig
    ];
    shellHook = ''
      export LIBCLANG_PATH="${pkgs.llvmPackages.libclang}/lib";
    '';
  }
