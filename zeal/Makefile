
PROJ_NAME ?= zeal
BUILDDIR := targets
CC := clang
CXX := clang++

all: build

.PHONY: all build run test clean help


help:
	@echo "Usage: make [target]"
	@echo ""
	@echo "Targets: "
	@echo "    build           Configure (if needed) and build the project in debug(dev) mode"
	@echo "    buildrelease    Configure (if needed) and build the project in release mode"
	@echo "    setup           Configure (if needed)  the project in debug(dev) mode"
	@echo "    setuprelease    Configure (if needed)  the project in release mode"
	@echo "    run             Run main executable in debug(dev) mode."
	@echo "    runrelease      Run main executable in release mode."
	@echo "    test            Run project's test suite."
	@echo "    install         Install this project to system"
	@echo "    cleandev        Remove the debug build directory."
	@echo "    cleanrelease        Remove the release build directory."
	@echo "    cleanup         Remove entire build directory."
	@echo "    refresh         clean build directory and reconfigure project"
	@echo "    help            Show this help message."


setuprelease:
	@if [ ! -f "$(BUILDDIR)/debug/build.ninja" ]; then \
		echo "Configuring Meson Project in Release mode..."; \
		meson setup "$(BUILDDIR)/release" --buildtype=release; \
	else \
		echo "Meson Project already configured. Build files already exist."; \
	fi  
	@echo "Building Project..."

setup:
	@if [ ! -f "$(BUILDDIR)/debug/build.ninja" ]; then \
		echo "Configuring Meson Project in Debug(dev) mode..."; \
		meson setup "$(BUILDDIR)/debug" --buildtype=debug; \
	else \
		echo "Meson Project already configured. Build files already exist."; \
	fi  
	@echo "Building Project..."


build: setup	
	@echo "Building Project in Debug(dev) mode...";
	@meson compile -C "$(BUILDDIR)/debug";
	# @if [ ! -f "$(BUILDDIR)/debug/build.ninja" ]; then \
	# 	echo "Configuring Meson Project in Debug(dev) mode..."; \
	# 	meson setup "$(BUILDDIR)/debug" ; \
	# else \
	# 	echo "Meson Project already configured. Build files already exist."; \
	# fi  
	# @echo "Building Project..."



buildrelease: setuprelease
	@echo "Building Project in Release mode...";
	@meson compile -C "$(BUILDDIR)/release";
	# @if [ ! -f "$(BUILDDIR)/release/build.ninja" ]; then \
	# 	echo "Configuring Meson Project in Release mode..."; \
	# 	meson setup "$(BUILDDIR)/release" --buildtype=release;
	# else \
	# 	echo "Meson Project already configured. Build files already exist."; \
	# fi
	# @echo "Building Project..."


install: buildrelease
	@echo "Installing project to system!"
	@meson install -C $(BUILDDIR)/release
	

run: build
	@echo "Running debug executable..."
	@$(BUILD_DIR)/debug/$(PROJ_NAME)


runrelease: buildrelease
	@echo "Running release executable..."
	@$(BUILD_DIR)/release/$(PROJ_NAME)

test: build
	@echo "Running Tests..."
	@meson test -C $(BUILDDIR)/debug --print-errorlogs

cleandev:
	@echo "Cleaning debug build directory..."
	@rm -rf $(BUILDDIR)/debug	


cleanrelease:
	@echo "Cleaning release build directory..."
	@rm -rf $(BUILDDIR)/release	

	
cleanup: 
	@echo "Cleaning entire build directory..."
	@rm -rf $(BUILDDIR)/
	


