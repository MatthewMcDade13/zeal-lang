cmake_minimum_required(VERSION 3.20)

# Enable compilation database for clangd
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

project(zvm VERSION 0.0.1 LANGUAGES CXX)

# Specify C++20
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Force Clang compiler
set(CMAKE_CXX_COMPILER clang++)
set(CMAKE_C_COMPILER clang)

# Set compile options
add_compile_options(
    -Wall
    -Wextra
    -Wpedantic
    -Werror
)

# Add subdirectories
add_subdirectory(src)
add_subdirectory(libs)
add_subdirectory(tests)
