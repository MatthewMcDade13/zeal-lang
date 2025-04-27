#!/bin/bash

ROOT_DIR="$(pwd)"

if [ -d "$ROOT_DIR"/builddir ]; then
  echo "Running Zeal Virtual Machine Repl!"
  exec "$ROOT_DIR"/builddir/zeal
else
  echo "Cannot run zeal without running $ROOT_DIR/build.sh!"
  exit 1
fi


