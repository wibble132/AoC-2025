cmake_minimum_required(VERSION 4.0)
project(cpp)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_executable(cpp main.cpp
        days/day01.cpp
        days/day01.h
        days/utils.h
        days/day03.cpp
        days/day03.h)

target_compile_options(cpp PRIVATE -Wall -Wextra -Wpedantic -Werror -O3)

set(BIN_DIR ${CMAKE_SOURCE_DIR}/bin)
add_custom_command(TARGET ${CMAKE_PROJECT_NAME}
        POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${CMAKE_PROJECT_NAME}> ${BIN_DIR}/${CMAKE_PROJECT_NAME}
        COMMENT "Copying binary files to ${BIN_DIR}"
)
