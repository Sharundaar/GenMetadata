#pragma once

#include <vector>
#include "object.h"

struct MyStruct : public Object
{
    GENERATE_BODY( MyStruct );

    u32 number1;
    i8  number2;
    std::vector<i32> some_array;
};

struct MyStruct2 : public Object
{
    GENERATE_BODY( MyStruct2 );

    const char* some_char;
    MyStruct myStruct;
};

class PhysicSystem
{
    int some_data;
    void update( const std::vector<MyStruct>& structs, const std::vector<MyStruct2>& structs2 );
};

void sys_physic( std::vector<MyStruct> my_structs );
void sys_graphic( std::vector<MyStruct2> my_structs );
void sys_gameplay( std::vector<MyStruct> my_structs, std::vector<MyStruct2> my_structs_2 );

bool test_function( int param1, float param2 );
