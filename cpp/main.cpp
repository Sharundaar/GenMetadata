#include <iostream>
#include <string>
#include <cstring>

#include "type_db.h"
#include "test.h"

using namespace std;

u8 s_alloc_buffer[0x4000];
u32 s_alloc_index = 0;
TypeInfo& alloc_type( TypeInfo_Type type, void* buffer )
{
    TypeInfo* alloc = nullptr;
    u8* byte_buffer = (u8*)buffer;
    switch( type )
    {
    case TypeInfo_Type::SCALAR:
        alloc = new (byte_buffer+s_alloc_index) ScalarInfo();
        s_alloc_index += sizeof( ScalarInfo );
        break;
    case TypeInfo_Type::FUNCTION:
        alloc = new (byte_buffer+s_alloc_index) FuncInfo();
        s_alloc_index += sizeof( FuncInfo );
        break;
    case TypeInfo_Type::ENUM:
        alloc = new (byte_buffer+s_alloc_index) EnumInfo();
        s_alloc_index += sizeof( EnumInfo );
        break;
    case TypeInfo_Type::STRUCT:
        alloc = new (byte_buffer+s_alloc_index) StructInfo();
        s_alloc_index += sizeof( StructInfo );
        break;
    case TypeInfo_Type::TEMPLATE:
        alloc = new (byte_buffer+s_alloc_index) TemplateInfo();
        s_alloc_index += sizeof( TemplateInfo );
        break;
    }
    return *alloc;
}

const TypeInfo* s_all_types[1024];
void init_type_system()
{
    register_types( alloc_type, s_alloc_buffer );
    u8* it = s_alloc_buffer;
    while( it < s_alloc_buffer + s_alloc_index )
    {
        auto type = (TypeInfo*)it;
        s_all_types[type->type_id.local_type] = type;
        switch( type->type )
        {
        case TypeInfo_Type::SCALAR:
            it += sizeof( ScalarInfo );
            break;
        case TypeInfo_Type::FUNCTION:
            it += sizeof( FuncInfo );
            break;
        case TypeInfo_Type::ENUM:
            it += sizeof( EnumInfo );
            break;
        case TypeInfo_Type::STRUCT:
            it += sizeof( StructInfo );
            break;
        case TypeInfo_Type::TEMPLATE:
            it += sizeof( TemplateInfo );
            break;
        }
    }
}

int main( int, char** )
{
    init_type_system();
    for( const auto** typePtr = &s_all_types[0]; *typePtr != nullptr; ++typePtr )
    {
        const auto* type = *typePtr;
        cout << "Type: " << type->name << endl;
        if( type->type == TypeInfo_Type::STRUCT )
        {
            const auto* struct_type = (StructInfo*) type;
            for( auto& member : struct_type->fields )
            {
                if( member.type )
                    cout << "\t" << member.offset << ": " << member.type->name << " " << member.name << endl;
                if( member.template_type )
                {
                    cout << "\t" << member.offset << ": " << member.template_type->definition->name << "( ";
                    for( auto& param: member.template_type->params )
                    {
                        if( param.info.modifier & FieldInfo_Modifier::CONSTANT )
                            cout << "const ";
                        if( param.info.type )
                            cout << param.info.type->name << ", ";
                    }
                    cout << ")" << endl;
                }
            }

            for( auto& func : struct_type->functions )
            {
                cout << "\t" << func.name << " -> " << (func.return_type ? func.return_type.type->name : "void") << endl;
            }
        }
    }

    return 0;
}