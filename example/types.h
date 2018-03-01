#pragma once

#include <vector>
#include <map>
#include <string>

#include "basic_types.h"

#define INVALID_TYPE_ID ((u32)0-1)

struct TypeInfo;
struct ScalarType;
struct MemberType;
struct StructType;

enum class TypeInfo_Type
{
    SCALAR,
    MEMBER,
    STRUCT,
    ENUM,
};

struct TypeInfo
{
    TypeInfo( std::string _name, u32 _size, TypeInfo_Type _type );

    std::string name;
    u32 size;
    TypeInfo_Type type;
    u32 type_id;

    const StructType* operator()() { return (StructType*)this; }
};

enum class ScalarType_Type
{
    INT,
    UINT,
    FLOAT,
};

struct ScalarType : public TypeInfo
{
    ScalarType( u32 _size, ScalarType_Type _scalar_type );
    static std::string get_name( u32 _size, ScalarType_Type _scalar_type );

    ScalarType_Type scalar_type;
};

enum class MemberType_Modifier
{
    NONE      = 0,
    PRIVATE   = 1 << 0,
    CONSTANT  = 1 << 1,
    POINTER   = 1 << 2,
    REFERENCE = 1 << 3,
};

struct MemberType : public TypeInfo
{
    MemberType( const std::string& _name, const TypeInfo* _member_type, MemberType_Modifier _modifier, u32 _offset );

    const TypeInfo* member_type;
    MemberType_Modifier modifier;
    u32 offset;
};

struct ObjectData
{
    u32 object_id;
};

struct StructType : public TypeInfo
{
    StructType( const std::string& _name, u32 _size, const StructType* _parent, std::vector<MemberType> _members );

    const std::vector<MemberType> members;
    const StructType* parent;
    const bool is_object;
    const ObjectData object_data;
};

struct EnumType : public TypeInfo
{
    EnumType( const std::string& _name, const TypeInfo* _underlying_type, std::map<std::string, i64> _enum_values );

    std::map<std::string, i64> enum_values;
    const TypeInfo* underlying_type;
};

#define MAX_TYPE_COUNT 1024
extern const TypeInfo* s_all_types[MAX_TYPE_COUNT];
extern const StructType* s_object_types[MAX_TYPE_COUNT];
template<typename T> const TypeInfo* type_of();
// const TypeInfo* type_of( const T& obj ); // for completness, this one doesn't need to be predeclared
