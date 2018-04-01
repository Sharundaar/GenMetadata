#pragma once

#include <vector>
#include <array>
#include <map>
#include <string>

#include "basic_types.h"

#define INVALID_TYPE_ID ((u32)0-1)

struct TypeInfo;
struct ScalarInfo;
struct FieldInfo;
struct StructInfo;
struct TemplateInfo;
struct TemplateInstance;
struct TemplateParam;
struct TemplateInstanceRef;

#define MAX_TYPE_COUNT 1024
extern const TypeInfo* s_all_types[MAX_TYPE_COUNT];
extern const StructInfo* s_object_types[MAX_TYPE_COUNT];
template<typename T> const TypeInfo* type_of();
const TypeInfo* get_type( u32 type_id );
const StructInfo* get_object_type( u32 type_id );
// const TypeInfo* type_of( const T& obj ); // for completness, this one doesn't need to be predeclared

struct VariadicSingleValue
{
    union
    {
        i8  int_8;
        i16 int_16;
        i32 int_32;
        i64 int_64;
        u8  uint_8;
        u16 uint_16;
        u32 uint_32;
        u64 uint_64;
        f32 float_32;
        f64 float_64;
    };
};

enum class TypeInfo_Type
{
    SCALAR,
    STRUCT,
    ENUM,
    TEMPLATE,
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

enum FieldInfo_Modifier
{
    NONE      = 0,
    PRIVATE   = 1 << 0,
    CONSTANT  = 1 << 1,
    POINTER   = 1 << 2,
    REFERENCE = 1 << 3,
    TEMPLATE  = 1 << 4,
};

struct FieldInfo
{
    FieldInfo();
    FieldInfo( const TypeInfo* _type, FieldInfo_Modifier _modifier, u32 _offset );
    FieldInfo( const std::string& _name, const TypeInfo* _type, FieldInfo_Modifier _modifier, u32 _offset );
    FieldInfo( const std::string& _name, const TemplateInstance* _template_type, FieldInfo_Modifier _modifier, u32 _offset );

    std::string name;
    const TypeInfo* type;
    const TemplateInstance* template_type;
    FieldInfo_Modifier modifier;
    u32 offset;

    bool operator==( const FieldInfo& other ) const;
    operator bool() const;

    template<typename T>
    void set( void* obj, const T& value ) const
    {
        if( type_of<T>() == type )
        {
            u8* dest = ((u8*)obj)+offset;
            memcpy( dest, &value, type->size );
        }
        // @Error: should err on else...
    }

    template<typename T>
    T get( void* obj ) const
    {
        T val;

        if( type_of<T>() == type )
        {
            u8* src = ((u8*)obj)+offset;
            memcpy( &val, src, type->size );
        }
        // @Error: should err on else...

        return val;
    }
};

struct FuncInfo
{
    FuncInfo( const std::string& _name, const TypeInfo* _return_type, const std::vector<FieldInfo>& _parameters );

    std::string name;
    const TypeInfo* return_type;
    const std::vector<FieldInfo> parameters;
};

struct ObjectData
{
    u32 object_id;
};

struct StructInfo : public TypeInfo
{
    StructInfo( const std::string& _name, u32 _size, const StructInfo* _parent, std::vector<FieldInfo> _fields, std::vector<FuncInfo> _functions );

    const FieldInfo& get_field( const std::string& field_name ) const;
    const FuncInfo& get_func( const std::string& functione_name ) const;

    const StructInfo* parent;
    const std::vector<FieldInfo> fields;
    const std::vector<FuncInfo> functions;
    const bool is_object;
    const ObjectData object_data;
};

struct EnumInfo : public TypeInfo
{
    EnumInfo( const std::string& _name, const TypeInfo* _underlying_type, std::map<std::string, i64> _enum_values );

    std::map<std::string, i64> enum_values;
    const TypeInfo* underlying_type;
};

struct TemplateParam
{
    const FieldInfo info;
    VariadicSingleValue instance_value; // depends on type_info
};

struct TemplateInstance
{
    TemplateInstance( const TemplateInfo* _definition, const std::array<TemplateParam, 4>& _params );

    const std::array<TemplateParam, 4> params;
    const TemplateInfo*                definition;
};

struct TemplateInstanceRef
{
    const TemplateInfo* definition;
    i32                 inst_idx;
    const TemplateInstance* operator->();
};

struct TemplateInfo : public TypeInfo
{
    TemplateInfo( const std::string& name );
    std::vector<TemplateInstance> instances;

    bool                has_instance( const std::array<TemplateParam, 4>& params );
    TemplateInstanceRef get_instance( const std::array<TemplateParam, 4>& params, bool create_if_needed = false );

private:
    i32 get_instance_internal( const std::array<TemplateParam, 4>& params );
};
