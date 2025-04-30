#!/bin/bash

WORKDIR="$(pwd)"
echo "$WORKDIR"

# Check if this directory has README.md
# as this is always in root of repo
if [ ! -d "$WORKDIR"/zeal ]; then
  echo "Can only run dev scripts from root project directory root!"
  exit 1
fi

pushd "$WORKDIR"/zeal


if [ "$1" = "release" ]; then
 BUILDTYPE="release" 
else
 BUILDTYPE="debug"
fi
 
# Possible buildtypes
#{plain,debug,debugoptimized,release,minsize,custom


echo "Builing zeal in mode: $BUILDTYPE"

if [ ! -d "$WORKDIR"/zeal/targets ]; then
  mkdir "$WORKDIR"/zeal/targets
fi

OUTDIR="$ROOTDIR"/targets/bin-"$BUILDDIR"

meson setup "$OUTDIR" --buildtype="$BUILDTYPE"

meson compile -C "$OUTDIR"

popd
