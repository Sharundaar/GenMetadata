#pragma once

#include <vector>
#include <map>
#include <string>

#include "basic_types.h"

struct TypeInfo;
struct ScalarType;
struct MemberType;
struct StructType;

struct TypeInfo
{
    enum class Type
    {
        SCALAR,
        MEMBER,
        STRUCT,
        ENUM,
    };

    TypeInfo( std::string _name, u32 _size, TypeInfo::Type _type );

    std::string name;
    u32 size;
    TypeInfo::Type type;

    const StructType* operator()() { return (StructType*)this; }
};

struct ScalarType : public TypeInfo
{
    enum class Type
    {
        INT,
        UINT,
        FLOAT,
    };

    ScalarType( u32 _size, ScalarType::Type _scalar_type );
    static std::string get_name( u32 _size, ScalarType::Type _scalar_type );

    ScalarType::Type scalar_type;
};

struct MemberType : public TypeInfo
{
    enum class Modifier {
        PRIVATE   = 1 << 0,
        CONST     = 1 << 1,
        POINTER   = 1 << 2,
        REFERENCE = 1 << 3,
    };

    MemberType( const std::string& _name, const TypeInfo* _member_type, Modifier _modifier );

    const TypeInfo* member_type;
    Modifier _modifier;
};

struct StructType : public TypeInfo
{
    StructType( const std::string& _name, u32 _size, const StructType* _parent, std::vector<MemberType> _members );
    static u32 generate_struct_id();

    const std::vector<MemberType> members;
    const StructType* parent;
    const u32 struct_id;
};

struct EnumType : public TypeInfo
{
    EnumType( const std::string& _name, const TypeInfo* _underlying_type, std::map<std::string, i64> _enum_values );

    std::map<std::string, i64> enum_values;
    const TypeInfo* underlying_type;
};

#define MAX_TYPE_COUNT 1024
extern const StructType* s_registered_structs[MAX_TYPE_COUNT];
template<typename T> const TypeInfo* type_of();
// const TypeInfo* type_of( const T& obj ); // for completness, this one doesn't need to be predeclared
