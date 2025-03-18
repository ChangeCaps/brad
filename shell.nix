{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.llvm_19
    pkgs.llvm_20
    pkgs.libxml2
    pkgs.lld
    pkgs.clang-tools
    pkgs.clang
    pkgs.valgrind
    pkgs.libsForQt5.kcachegrind

    pkgs.lua
  ];
}
