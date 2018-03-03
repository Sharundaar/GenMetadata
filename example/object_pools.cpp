#include "object_pools.h"

#include <iostream>

std::array<MemoryPoolPtr, MAX_TYPE_COUNT> s_pools = { nullptr };

MemoryPoolPtr get_pool( Object* obj )
{
    auto struct_info = obj->get_type();
    return s_pools[struct_info->object_data.object_id];
}

void show_pool_report()
{
    for( size_t i=0; i<s_pools.size(); ++i )
    {
        if( s_pools[i] == nullptr )
            continue;

        std::cout << "Pool found for type: " << get_object_type( (u32) i )->name << std::endl; 
    }
}