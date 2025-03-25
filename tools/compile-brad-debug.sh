#!/bin/bash

set -e

if [ "$#" -lt 1 ]; then
  echo "Usage: $0 package1 [package2 ... packageN]"
  exit 1
fi


cargo run -- v1 llvm-ir "$@" -o out.ll

last_pkg=$(eval echo "\${$#}")
entry_module=$(basename "$last_pkg" .bd)

cat <<EOF >> out.ll
define i32 @main() {
    %call = call i32 @"${entry_module}::main"()
    ret i32 0
}
EOF

# Compile IR to object file
clang -o a.out out.ll -ggdb -g -Og obj/debug/*.o
rm out.ll

# Run compiled binary
./a.out
