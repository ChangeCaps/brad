{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.llvm
    pkgs.libxml2
    pkgs.clang
    pkgs.clang-tools
  ];
}
