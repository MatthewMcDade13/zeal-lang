#!/bin/bash

ROOT_DIR="$(pwd)"

if [ ! -d "$ROOT_DIR"/builddir ]; then
  echo "Did not find build directory. creating one at: $ROOT_DIR/builddir"
  meson setup builddir
fi


echo "Compiling Project"
meson compile -C "$ROOT_DIR"/builddir


