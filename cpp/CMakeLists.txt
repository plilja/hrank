cmake_minimum_required(VERSION 3.3)
project(cpp)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

add_executable(count_luck count_luck/count_luck.cpp)
add_executable(counter_game counter_game/counter_game.cpp)
