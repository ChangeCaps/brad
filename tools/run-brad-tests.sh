#!/bin/bash

mkdir -p .logs

for file in tests/*.bd; do
   # redirect stderr to stdout
  (bash ./tools/compile-brad-debug.sh lib/v1/std $file) > .logs/run.log 2>&1
  exit_code=$?

  if [ $exit_code -ne 0 ]; then
    >&2 echo "Test $file failed with exit code $exit_code see run.log for more details"
    filename=$(basename -- "$file")
    mv .logs/run.log .logs/run-$filename.failed.log
  else
    echo "Test $file passed"
  fi

  rm .logs/run.log
done
