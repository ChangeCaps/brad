{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.llvm_19
    pkgs.libxml2
    pkgs.lld
    pkgs.clang-tools
    pkgs.clang
    pkgs.valgrind
    pkgs.libsForQt5.kcachegrind
    pkgs.liburing
    pkgs.binutils
    pkgs.nasm

    pkgs.lua
  ];
}
