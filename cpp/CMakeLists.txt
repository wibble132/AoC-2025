cmake_minimum_required(VERSION 4.0)
project(cpp)

set(CMAKE_CXX_STANDARD 23)

add_executable(cpp main.cpp
        days/day01.cpp
        days/day01.h
        days/utils.h)
