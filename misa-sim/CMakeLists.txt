cmake_minimum_required(VERSION 3.20)

project(misa-sim C)

add_compile_options(-std=c2x -Wall -Wextra -Wpedantic -Werror)

add_executable(misa-sim
  src/main.c
  src/logger.c
)
target_include_directories(misa-sim PRIVATE include)
