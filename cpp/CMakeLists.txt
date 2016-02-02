cmake_minimum_required(VERSION 3.3)
project(cpp)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

add_executable(count_luck count_luck/count_luck.cpp)
add_executable(counter_game counter_game/counter_game.cpp)
add_executable(dijkstrashortreach dijkstrashortreach/dijkstrashortreach.cpp)
add_executable(sansa sansa_and_xor/sansa.cpp)
add_executable(cipher cipher/cipher.cpp)
add_executable(coin_change coin_change/coin_change.cpp)
add_executable(team_formation team_formation/team_formation.cpp)
