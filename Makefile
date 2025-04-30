# Use .PHONY to declare targets that aren't actual files.
# This ensures 'make' runs the commands even if a file named 'build', 'run', etc., exists.
.PHONY: build run clean run-release all

# Default target (runs when you just type 'make')
all: build run

build:
	@echo "Building the project in dev mode..."
	./dev-scripts/native/build.sh dev
	@echo "Dev Build Complete"
run:
	@echo "Running zeal in dev (debug) mode..."
	./dev-scripts/native/run.sh dev
	@echo "Run Dev Complete"

run-release:
	@echo "Building release version "
	./dev-scripts/native/build.sh release
	@echo "Running zeal in release mode"
	./dev-scripts/native/run.sh release
	@echo "Run release complete."

clean:
	@echo "Cleaning up build files..."
	 rm -rf ./targets/
	@echo "Clean complete."
