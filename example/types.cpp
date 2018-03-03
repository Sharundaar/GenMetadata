#include "types.h"

#include "object.h"

static u32 generate_type_id()
{
    static u32 s_high_type_id = 0;
    return s_high_type_id++;
}

TypeInfo::TypeInfo( const std::string& _name, u32 _size, TypeInfo_Type _type )
    : name( _name ), size( _size ), type( _type ), type_id( generate_type_id() )
{
    if( type_id != INVALID_TYPE_ID )
        s_all_types[type_id] = this;
}

ScalarInfo::ScalarInfo( u32 _size, ScalarInfo_Type _scalar_type )
    : TypeInfo( ScalarInfo::get_name( _size, _scalar_type ), _size, TypeInfo_Type::SCALAR ), scalar_type( _scalar_type )
{
}

std::string ScalarInfo::get_name( u32 _size, ScalarInfo_Type _scalar_type )
{
    switch( _scalar_type )
    {
    case ScalarInfo_Type::INT:
        if( _size == sizeof( i8  ) ) return "i8";
        if( _size == sizeof( i16 ) ) return "i16";
        if( _size == sizeof( i32 ) ) return "i32";
        if( _size == sizeof( i64 ) ) return "i64";
        break;
    case ScalarInfo_Type::UINT:
        if( _size == sizeof( u8  ) ) return "u8";
        if( _size == sizeof( u16 ) ) return "u16";
        if( _size == sizeof( u32 ) ) return "u32";
        if( _size == sizeof( u64 ) ) return "u64";
        break;
    case ScalarInfo_Type::FLOAT:
        if( _size == sizeof( f32 ) ) return "f32";
        if( _size == sizeof( f64 ) ) return "f64";
        break;
    default:
        break;
    }

    return ""; // should not reach here
}

FieldInfo::FieldInfo( const std::string& _name, const TypeInfo* _type, FieldInfo_Modifier _modifier, u32 _offset )
    : name(_name), type( _type ), modifier( _modifier ), offset( _offset )
{
}

EnumInfo::EnumInfo( const std::string& _name, const TypeInfo* _underlying_type, std::map<std::string, i64> _enum_values )
    : TypeInfo( _name, _underlying_type->size, TypeInfo_Type::ENUM ), enum_values( _enum_values ), underlying_type( _underlying_type )
{
}

static u32 generate_object_id()
{
    static u32 s_high_type_id = 0;
    return s_high_type_id++;
}

static bool is_object_func( const StructInfo* type_info )
{
    return type_info != nullptr && ( type_info == type_of<Object>() || is_object_func( type_info->parent ) );
}

static ObjectData from_struct_type( const StructInfo* struct_type )
{
    if( !struct_type->is_object )
        return ObjectData{ INVALID_TYPE_ID };
    ObjectData data = ObjectData { generate_object_id() };
    s_object_types[ data.object_id ] = struct_type;
    return data;
}

StructInfo::StructInfo( const std::string& _name, u32 _size, const StructInfo* _parent, std::vector<FieldInfo> _members )
    : TypeInfo( _name, _size, TypeInfo_Type::STRUCT ), parent(_parent), members(_members), is_object( is_object_func(this) ), object_data( from_struct_type(this) )
{
}

const TypeInfo* s_all_types[MAX_TYPE_COUNT];
const StructInfo* s_object_types[MAX_TYPE_COUNT];