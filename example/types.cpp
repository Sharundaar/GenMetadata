#include "types.h"

#include "object.h"

static u32 generate_type_id()
{
    static u32 s_high_type_id = 0;
    return s_high_type_id++;
}

TypeInfo::TypeInfo( std::string _name, u32 _size, TypeInfo_Type _type )
    : name( _name ), size( _size ), type( _type ), type_id( _type == TypeInfo_Type::MEMBER ? INVALID_TYPE_ID : generate_type_id() )
{
    if( type_id != INVALID_TYPE_ID )
        s_all_types[type_id] = this;
}

ScalarType::ScalarType( u32 _size, ScalarType_Type _scalar_type )
    : TypeInfo( ScalarType::get_name( _size, _scalar_type ), _size, TypeInfo_Type::SCALAR ), scalar_type( _scalar_type )
{
}

std::string ScalarType::get_name( u32 _size, ScalarType_Type _scalar_type )
{
    switch( _scalar_type )
    {
    case ScalarType_Type::INT:
        if( _size == sizeof( i8  ) ) return "i8";
        if( _size == sizeof( i16 ) ) return "i16";
        if( _size == sizeof( i32 ) ) return "i32";
        if( _size == sizeof( i64 ) ) return "i64";
        break;
    case ScalarType_Type::UINT:
        if( _size == sizeof( u8  ) ) return "u8";
        if( _size == sizeof( u16 ) ) return "u16";
        if( _size == sizeof( u32 ) ) return "u32";
        if( _size == sizeof( u64 ) ) return "u64";
        break;
    case ScalarType_Type::FLOAT:
        if( _size == sizeof( f32 ) ) return "f32";
        if( _size == sizeof( f64 ) ) return "f64";
        break;
    default:
        break;
    }

    return ""; // should not reach here
}

MemberType::MemberType( const std::string& _name, const TypeInfo* _member_type, MemberType_Modifier _modifier, u32 _offset )
    : TypeInfo( _name, _member_type->size, TypeInfo_Type::MEMBER ), member_type( _member_type ), modifier( _modifier ), offset( _offset )
{
}

EnumType::EnumType( const std::string& _name, const TypeInfo* _underlying_type, std::map<std::string, i64> _enum_values )
    : TypeInfo( _name, _underlying_type->size, TypeInfo_Type::ENUM ), enum_values( _enum_values ), underlying_type( _underlying_type )
{
}

static u32 generate_object_id()
{
    static u32 s_high_type_id = 0;
    return s_high_type_id++;
}

static bool is_object_func( const StructType* type_info )
{
    return type_info != nullptr && ( type_info == type_of<Object>() || is_object_func( type_info->parent ) );
}

static ObjectData from_struct_type( const StructType* struct_type )
{
    if( !struct_type->is_object )
        return ObjectData{ INVALID_TYPE_ID };
    ObjectData data = ObjectData { generate_object_id() };
    s_object_types[ data.object_id ] = struct_type;
}

StructType::StructType( const std::string& _name, u32 _size, const StructType* _parent, std::vector<MemberType> _members )
    : TypeInfo( _name, _size, TypeInfo_Type::STRUCT ), parent(_parent), members(_members), is_object( is_object_func(this) ), object_data( from_struct_type(this) )
{
}

const TypeInfo* s_all_types[MAX_TYPE_COUNT];
const StructType* s_object_types[MAX_TYPE_COUNT];