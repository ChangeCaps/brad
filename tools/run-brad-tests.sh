#!/bin/bash

for file in tests/*.bd; do
  ./tools/compile-brad-debug.sh std $file 2> /dev/null > /dev/null

  if [ $? -ne 0 ]; then
    >&2 echo "Test $file failed with exit code $?"
    exit 1
  else
    echo "Test $file passed"
  fi
done