#pragma once

#include "types.h"
#include "basic_types.h"

#define GENERATE_BODY( Struct ) \
    Struct();                   \
    Struct( TypeId _type_id );  \
    Object* operator()() { return (Object*) this; }

struct Object
{
    Object( TypeId _type_id ) : m_type_id( _type_id ) {}
    TypeId m_type_id;
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