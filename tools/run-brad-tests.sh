#!/bin/bash

for file in tests/*.bd; do
   # redirect stderr to stdout
   bash ./tools/compile-brad-debug.sh std $file 2>1 > run.log

  if [ $? -ne 0 ]; then
    >&2 echo "Test $file failed with exit code $? see run.log for more details"
    exit 1
  else
    echo "Test $file passed"
  fi
done
