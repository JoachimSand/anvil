
let
  pkgs = import <nixpkgs> { };
in
  with pkgs;
  stdenv.mkDerivation {
    name = "clang-env-with-nightly-rust";
    buildInputs = [
      libllvm
      libz
    ];
    # why do we need to set the library path manually?
    shellHook = ''
      export LIBCLANG_PATH="${pkgs.llvmPackages.libclang}/lib";
    '';
  }
