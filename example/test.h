#pragma once

#include <vector>
#include "object.h"

struct MyStruct : public Object
{
    GENERATE_BODY( MyStruct );

    u32 number1 = 0;
    i8  number2 = 0;
    bool some_bool = false;
    char some_char = 'c';
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
    u32 some_data;
    std::vector<MyStruct> myStructs;
    std::vector<MyStruct2> myStructs2;
    void update( const std::vector<MyStruct>& structs, const std::vector<MyStruct2>& structs2 );
};

void sys_physic( std::vector<MyStruct> my_structs );
void sys_graphic( std::vector<MyStruct2> my_structs );
void sys_gameplay( std::vector<MyStruct> my_structs, std::vector<MyStruct2> my_structs_2 );

bool test_function( int param1, float param2 );
