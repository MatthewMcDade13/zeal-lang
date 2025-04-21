
WORKDIR="$(pwd)"

if [ ! -d "$WORKDIR/build" ]; then 
  echo "Creating build directory: $WORKDIR/build"

  mkdir -p "$WORKDIR/build" || {
    echo "Failed to create build directory"
    exit 1 
  }

fi

pushd build

cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ..
cmake --build .


popd

