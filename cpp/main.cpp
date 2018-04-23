#include <iostream>
#include <string>
#include <cstring>

#define TYPES_IMPLEMENTATION
#include "types.h"
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
        switch( type->type )
        {
        case TypeInfo_Type::SCALAR:
            s_all_types[type->type_id.local_type] = type;
            it += sizeof( ScalarInfo );
            break;
        case TypeInfo_Type::FUNCTION:
            it += sizeof( FuncInfo );
            break;
        case TypeInfo_Type::ENUM:
            s_all_types[type->type_id.local_type] = type;
            it += sizeof( EnumInfo );
            break;
        case TypeInfo_Type::STRUCT:
            s_all_types[type->type_id.local_type] = type;
            it += sizeof( StructInfo );
            break;
        case TypeInfo_Type::TEMPLATE:
            s_all_types[type->type_id.local_type] = type;
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
        switch( type->type )
        {
        case TypeInfo_Type::STRUCT: {
                cout << "Struct: " << type->name << endl;
                const auto* struct_type = (StructInfo*) type;
                for( auto& member : struct_type->fields )
                {
                    if( member.type )
                        cout << "\t" << member.offset << ": " << member.type->name << " " << member.name << endl;
                    if( member.template_type )
                    {
                        cout << "\t" << member.offset << ": " << member.template_type->definition->name << "( ";
                        for(int i=0; i < member.template_type->params.size(); ++i)
                        {
                            auto& param = member.template_type->params[i];
                            if( param.info.type )
                            {
                                if( i>0 )
                                    cout << ", ";
                                if( param.info.modifier & FieldInfo_Modifier::CONSTANT )
                                    cout << "const ";
                                cout << param.info.type->name;
                                if( param.info.modifier & FieldInfo_Modifier::POINTER )
                                    cout << "*";
                                if( param.info.modifier & FieldInfo_Modifier::REFERENCE )
                                    cout << "&";
                            }
                        }
                        cout << " )" << endl;
                    }
                }

                for( auto& func : struct_type->functions )
                {
                    cout << "\t" << func.name << " -> " << (func.return_type ? func.return_type.type->name : "void") << endl;
                }
                break;
            }
        case TypeInfo_Type::SCALAR: {
            cout << "Scalar: " << type->name << endl;
            break;
        }
        case TypeInfo_Type::FUNCTION: {
            cout << "Function: " << type->name << endl;
            break;
        }
        case TypeInfo_Type::TEMPLATE: {
            cout << "Template: " << type->name << endl;
            break;
        }
        case TypeInfo_Type::ENUM: {
            cout << "Enum: " << type->name << endl;
            break;
        }
        }
    }

    return 0;
}