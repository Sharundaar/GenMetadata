#pragma once

#include "types.h"
#include "basic_types.h"
#include <vector>

#define GENERATE_BODY( Struct ) \
    Struct();                   \
    Struct( u32 _type_id );     \
    Object* operator()() { return (Object*) this; }

struct Object
{
    Object( u32 _object_id ) : object_id( _object_id ) {}
    u32 object_id;
    const StructInfo* get_type() { return s_object_types[ object_id ]; }
};

struct MyStruct : public Object
{
    GENERATE_BODY( MyStruct )

    u32 number1;
    i8  number2;
    std::vector<i32> some_array;
};
