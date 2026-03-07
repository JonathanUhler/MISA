cmake_minimum_required(VERSION 3.20)

project(misa-sim C)

add_compile_options(-std=c2x -Wall -Wextra -Wpedantic -Werror)

add_executable(misa-sim
  src/csrfile.c
  src/logger.c
  src/main.c
  src/memory.c
  src/processor.c
  src/regfile.c
)
target_include_directories(misa-sim PRIVATE include)
