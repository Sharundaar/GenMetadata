cmake_minimum_required(VERSION 3.0.0)
project(TypeInfo VERSION 0.0.0)

set(SRCS 
         basics.cpp
)
set(LIBRARY_OUTPUT_PATH ../bin)
set(EXECUTABLE_OUTPUT_PATH ../bin)

add_library(TypeInfo STATIC ${SRCS})
add_executable(TypeInfoTest type_db.cpp main.cpp)

target_link_libraries(TypeInfoTest TypeInfo)
target_compile_features(TypeInfoTest PRIVATE cxx_generalized_initializers)