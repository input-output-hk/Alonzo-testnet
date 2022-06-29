{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
   packages = [
     pkgs.haskell.compiler.ghc8107
     pkgs.libsodium.dev
     pkgs.pkg-config
     pkgs.zlib
   ];
}
