#pragma once

#include <string>
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

    unsigned int some_unsigned_int;
    unsigned char some_unsigned_char;
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

template<typename T>
class TemplateClass
{
    T data;
};

template<typename T, int I>
struct TemplateStruct
{
    T data[I];
};

struct SomeStruct
{
    TemplateClass<const u32*> cl;
    TemplateStruct<char, 23> ts;
    std::string str;
    u32 i;
};

class SomeClass
{
    char c;
    void some_function();
    void some_function2( u32 a );
    u32  some_function3( u32 b );
    int  some_error_function( int a );
};
