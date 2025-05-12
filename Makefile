# Makefile for a Meson-based C++ project

# Variables
# Build directory (default is 'builddir')
BUILD_DEBUG ?= targets/debug
BUILD_RELEASE ?= targets/release

# Name of the main executable produced by your Meson project
# IMPORTANT: Change this to your project's executable name
EXECUTABLE_NAME ?= zeal

# Default target: build the project
all: build

# Configure the project using Meson
# This only needs to be run once, or if meson.build changes
setup-dev:
	@if [ ! -d "$(BUILD_DEBUG)" ]; then \
		echo "Configuring  Meson Debug project in $(BUILD_DEBUG)..."; \
		meson setup $(BUILD_DEBUG); \
	else \
		echo "Build directory $(BUILD_DEBUG) already exists. Skipping Debug configure."; \
		echo "Run 'make setup-dev' to force reconfiguration."; \
	fi

# Reconfigure the project (e.g., if meson.build or options change)
reconfigure-dev:
	@echo "Reconfiguring Meson project in $(BUILD_DEBUG)..."
	meson setup --reconfigure $(BUILD_DEBUG)

# Build the project
# Depends on the configure step implicitly because 'meson compile' needs a configured build directory
build-dev:
	@if [ ! -d "$(BUILD_DEBUG)" ]; then \
		echo "Debug Build directory not found. Running 'make setup-dev' first..."; \
		make configure; \
	fi
	@echo "Building Debug project in $(BUILD_DEBUG)..."
	meson compile -C $(BUILD_DEBUG)

# Run the main executable
# Depends on the build step
run-dev: build-dev
	@echo "Running Zeal in Debug"
	$(BUILD_DEBUG)/zeal

# Run tests
# Depends on the build step
test: build-dev
	@echo "Running tests in $(BUILD_DEBUG)..."
	meson test -C $(BUILD_DEBUG)

# Install the project
# Depends on the build step
install: build
	@echo "Installing project from $(BUILD_DEBUG)..."
	meson install -C $(BUILD_DEBUG)

# Clean the build directory
clean-dev:
	@echo "Cleaning build directory $(BUILD_DEBUG)..."
	rm -rf $(BUILD_DEBUG)

# Configure the project using Meson
# This only needs to be run once, or if meson.build changes
setup:
	@if [ ! -d "$(BUILD_RELEASE)" ]; then \
		echo "Configuring  Meson Debug project in $(BUILD_RELEASE)..."; \
		meson setup $(BUILD_RELEASE); \
	else \
		echo "Release Build directory $(BUILD_RELEASE) already exists. Skipping Release configure."; \
		echo "Run 'make setup' to force reconfiguration."; \
	fi

# Reconfigure the project (e.g., if meson.build or options change)
reconfigure:
	@echo "Reconfiguring Meson project in Release directory: $(BUILD_RELEASE)..."
	meson setup --reconfigure $(BUILD_RELEASE)

# Build the project
# Depends on the configure step implicitly because 'meson compile' needs a configured build directory
build:
	@if [ ! -d "$(BUILD_RELEASE)" ]; then \
		echo "Release Build directory not found. Running 'make setup' first..."; \
		make configure; \
	fi
	@echo "Building Debug project in $(BUILD_RELEASE)..."
	meson compile -C $(BUILD_RELEASE)

# Run the main executable
# Depends on the build step
run: build
	@echo "Running Zeal in Debug"
	$(BUILD_RELEASE)/zeal

# Clean the build directory
clean:
	@echo "Cleaning build Release directory $(BUILD_RELEASE)..."
	rm -rf $(BUILD_RELEASE)

	

# Display help
help:
	@echo "Zeal Programming Language Meson Project"
	@echo ""
	@echo "Usage: make [target]"
	@echo ""
	@echo "Targets:"
	@echo "  all (default) - Build the project."
	@echo "  setup-dev     - Set up the Meson Debug build directory (if it doesn't exist)."
	@echo "  reconfigure-dev   - Force re-configuration of the Meson Debug build directory."
	@echo "  build-dev         - Compile the project, in debug mode"
	@echo "  run-dev           - Build and run the main Debug executable"
	@echo "  test          - Build and run tests."
	@echo "  install       - Build and install the project."
	@echo "  clean-dev         - Remove the Debug build directory ($(BUILD_DEBUG))."

	@echo "  setup         - Set up the Meson Release build directory (if it doesn't exist)."
	@echo "  reconfigure   - Force re-configuration of the Meson Release build directory."
	@echo "  build         - Compile the project. in Release mode"
	@echo "  run           - Build and run the main executable in Release mode."
	@echo "  test          - Build and run tests."
	@echo "  install       - Build and install the project."
	@echo "  clean         - Remove the Release build directory ($(BUILD_RELEASE))."

	
	@echo "  help          - Show this help message."
	@echo ""
	@echo "Variables (can be overridden on the command line, e.g., make BUILD_DIR=my_build):"
	@echo "  BUILD_DEV       - Build directory (default: $(BUILD_DIR))"
#	@echo "  CXX             - C++ compiler (default: auto-detected by Meson)"
#	@echo "  MESON           - Meson command (default: $(MESON))"

# Phony targets (targets that are not actual files)
.PHONY: all setup setup-dev reconfigure reconfigure-dev build build-dev run run-dev test install clean clean-dev  help
