cmake_minimum_required(VERSION 3.0.0)
project(TypeInfo VERSION 0.0.0)

set(SRCS 
         basics.cpp
         main.cpp
         type_db.cpp
         object_pools.cpp
         types.cpp
)

add_executable(TypeInfo ${SRCS})
set(EXECUTABLE_OUTPUT_PATH ../bin)
target_compile_features(TypeInfo PRIVATE cxx_generalized_initializers)

set(CPACK_PROJECT_VERSION ${PROJECT_VERSION})
set(CPACK_PROJECT_NAME ${PROJECT_NAME})
include(CPack)