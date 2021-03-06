#include <iostream>
#include <string>
#include <cstring>

#define TYPES_IMPLEMENTATION
#include "types.h"
#include "type_db.h"
#include "test.h"

using namespace std;

u8 s_alloc_buffer[0x10000];
u32 s_type_alloc_index = 0;
u32 s_data_alloc_index = 0;
TypeInfo& alloc_type( void* buffer )
{
    TypeInfo* alloc = nullptr;
    u8* byte_buffer = (u8*)buffer;
    alloc = new (byte_buffer + s_type_alloc_index) TypeInfo();
    s_type_alloc_index += sizeof( TypeInfo );
    return *alloc;
}

void* alloc_data( void* buffer, uint32_t size )
{
    void* alloc = (u8*)buffer + s_data_alloc_index;
    s_data_alloc_index += size;
    return alloc;
}

const TypeInfo* s_all_types[1024];
void init_type_system()
{
    register_types( alloc_type, s_alloc_buffer, alloc_data, s_alloc_buffer + 0x5000);
    u8* it = s_alloc_buffer;
    while( it < s_alloc_buffer + s_type_alloc_index )
    {
        auto type = (TypeInfo*)it;
        if( type->type_id != INVALID_TYPE_ID )
            s_all_types[type->type_id] = type;
        it += sizeof( TypeInfo );
    }
}

int main( int, char** )
{
    init_type_system();

    for( const auto** typePtr = &s_all_types[0]; *typePtr != nullptr; ++typePtr )
    {
        const auto& type = **typePtr;
        switch( type.type )
        {
        case TypeInfoType::Struct: {
                cout << type.name << " :: " << type.struct_info.size << endl;
                const StructInfo& struct_type = type;
                for( u32 field_index = 0; field_index < struct_type.field_count; ++field_index )
                {
                    const auto& member = struct_type.fields[field_index];
                    if( member.type )
                    {
                        printf("\t[%d]\t %s%c %s\n", member.offset, member.type->name, ((member.modifier & FieldInfoModifier::POINTER) ? '*' : ((member.modifier & FieldInfoModifier::REFERENCE) ? '&' : ' ')), member.name);
                    }
                    else
                    {
                        printf("\t[%d]\t (unknown)%c %s\n", member.offset, ((member.modifier & FieldInfoModifier::POINTER) ? '*' : ((member.modifier & FieldInfoModifier::REFERENCE) ? '&' : ' ')), member.name);
                    }
                }

                for( u32 func_index = 0; func_index < struct_type.function_count; ++func_index )
                {
                    const auto& func = struct_type.functions[func_index];
                    cout << "\t" << func.name << " -> " << (func.func_info.return_type ? func.func_info.return_type.type->name : "void") << endl;
                }
                break;
            }
        /*
        case TypeInfoType::Scalar: {
            cout << "Scalar: " << type.name << endl;
            break;
        }
        case TypeInfoType::Function: {
            cout << "Function: " << type.name << endl;
            break;
        }
        case TypeInfoType::TemplateDef: {
            cout << "Template: " << type.name << endl;
            break;
        }
        case TypeInfoType::Enum: {
            cout << "Enum: " << type.name << " :: " << type.enum_info.underlying_type->scalar_info.size << endl;
            break;
        }
        case TypeInfoType::Typedef: {
            cout << "Typedef: " << type.name << endl;
            break;
        }*/
        }
    }

    return 0;
}