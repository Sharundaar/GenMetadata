#pragma once

#include <vector>
#include <map>
#include <string>

#include "basic_types.h"

#define INVALID_TYPE_ID ((u32)0-1)

struct TypeInfo;
struct ScalarInfo;
struct FieldInfo;
struct StructInfo;

enum class TypeInfo_Type
{
    SCALAR,
    STRUCT,
    ENUM,
};

struct TypeInfo
{
    TypeInfo( const std::string& _name, u32 _size, TypeInfo_Type _type );

    std::string name;
    u32 size;
    TypeInfo_Type type;
    u32 type_id;

    const StructInfo* operator()() { return (StructInfo*)this; }
};

enum class ScalarInfo_Type
{
    INT,
    UINT,
    FLOAT,
};

struct ScalarInfo : public TypeInfo
{
    ScalarInfo( u32 _size, ScalarInfo_Type _scalar_type );
    static std::string get_name( u32 _size, ScalarInfo_Type _scalar_type );

    ScalarInfo_Type scalar_type;
};

enum class FieldInfo_Modifier
{
    NONE      = 0,
    PRIVATE   = 1 << 0,
    CONSTANT  = 1 << 1,
    POINTER   = 1 << 2,
    REFERENCE = 1 << 3,
};

struct FieldInfo
{
    FieldInfo( const std::string& _name, const TypeInfo* _type, FieldInfo_Modifier _modifier, u32 _offset );

    std::string name;
    const TypeInfo* type;
    FieldInfo_Modifier modifier;
    u32 offset;
};

struct ObjectData
{
    u32 object_id;
};

struct StructInfo : public TypeInfo
{
    StructInfo( const std::string& _name, u32 _size, const StructInfo* _parent, std::vector<FieldInfo> _members );

    const std::vector<FieldInfo> members;
    const StructInfo* parent;
    const bool is_object;
    const ObjectData object_data;
};

struct EnumInfo : public TypeInfo
{
    EnumInfo( const std::string& _name, const TypeInfo* _underlying_type, std::map<std::string, i64> _enum_values );

    std::map<std::string, i64> enum_values;
    const TypeInfo* underlying_type;
};

#define MAX_TYPE_COUNT 1024
extern const TypeInfo* s_all_types[MAX_TYPE_COUNT];
extern const StructInfo* s_object_types[MAX_TYPE_COUNT];
template<typename T> const TypeInfo* type_of();
// const TypeInfo* type_of( const T& obj ); // for completness, this one doesn't need to be predeclared
