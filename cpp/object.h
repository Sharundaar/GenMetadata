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

template<typename T>
T* cast( Object* obj )
{
    if( type_of<T>() == obj->get_type() )
    {
        return static_cast<T*>( obj );
    }

    return nullptr;
}