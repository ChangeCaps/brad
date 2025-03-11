#!/bin/bash

for file in tests/*.bd; do
   # redirect stderr to stdout
  (bash ./tools/compile-brad-debug.sh std $file) > run.log 2>&1
  exit_code=$?

  if [ $exit_code -ne 0 ]; then
    >&2 echo "Test $file failed with exit code $exit_code see run.log for more details"
    exit 1
  else
    echo "Test $file passed"
  fi

  rm run.log
done
