#!/bin/bash

if [ "$#" -lt 1 ]; then
  echo "Usage: $0 package1 [package2 ... packageN]"
  exit 1
fi

make clean
make release

cargo run -r -- compile "$@" -d -o out.ll 2> /dev/null

last_pkg=$(eval echo "\${$#}")
entry_module=$(basename "$last_pkg" .bd)

cat <<EOF >> out.ll
define i32 @main() {
    %call = call i32 @"${entry_module}::main"()
    ret i32 0
}
EOF

# Compile IR to object file
llc -filetype=obj out.ll -o out.o
clang -o a.out out.o obj/*.o -static -fuse-ld=lld
rm out.ll out.o

# Run compiled binary
./a.out