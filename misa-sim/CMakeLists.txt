cmake_minimum_required(VERSION 3.20)

project(misa-sim LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
add_compile_options(-Wall -Wextra -Wpedantic -Werror)

include(FetchContent)
FetchContent_Declare(
  systemc
  GIT_REPOSITORY https://github.com/accellera-official/systemc.git
  GIT_TAG        2.3.4
)
FetchContent_MakeAvailable(systemc)

add_executable(misa-sim
  src/main.cpp
  src/arithmetic.cpp
  src/comparator.cpp
  src/csrfile.cpp
  src/memory.cpp
  src/regfile.cpp
)
target_include_directories(misa-sim PRIVATE include)
target_link_libraries(misa-sim systemc)
