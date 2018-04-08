#include "memory_pool.h"
#include "object.h"

extern std::array<MemoryPoolPtr, MAX_TYPE_COUNT> s_pools;

MemoryPoolPtr get_pool( Object* obj );

template<typename T>
void init_pool()
{
    auto type_info = type_of<T>();
    if( type_info->type == TypeInfo_Type::STRUCT )
    {
        auto struct_info = static_cast<const StructInfo*>(type_info);
        if( struct_info->is_object )
            s_pools[struct_info->object_data.object_id] = new MemoryPool< T, get_pool_size<T>() >();
    }
}

template<typename T>
MemoryPool<T, get_pool_size<T>()>* cast_pool( MemoryPoolPtr pool )
{
    return static_cast<MemoryPool<T, get_pool_size<T>()>*>( pool );
}

template<typename T>
MemoryPool<T, get_pool_size<T>()>* get_pool()
{
    auto type_info = type_of<T>();
    if( type_info->type == TypeInfo_Type::STRUCT )
    {
        auto struct_info = static_cast<const StructInfo*>(type_info);
        if( struct_info->is_object )
            return static_cast<MemoryPool<T, get_pool_size<T>()>*>( s_pools[struct_info->object_data.object_id] );
    }

    return nullptr;
}

template<typename T>
void delete_pool()
{
    auto type_info = type_of<T>();
    if( type_info->type == TypeInfo_Type::STRUCT )
    {
        auto struct_info = static_cast<const StructInfo*>(type_info);
        if( struct_info->is_object )
            delete s_pools[struct_info->object_data.object_id];
    }
}

void show_pool_report();
