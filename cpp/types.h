#pragma once

#include <vector>
#include <array>
#include <map>
#include <string>

#include "basic_types.h"

struct TypeId;
struct TypeInfo;
struct ScalarInfo;
struct EnumInfo;
struct FieldInfo;
struct StructInfo;
struct FuncInfo;
struct TemplateInfo;
struct TemplateInstance;
struct TemplateParam;
struct TemplateInstanceRef;

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
    FUNCTION,
};

struct TypeId
{
    u32 global_type = 0;
    u32 local_type  = 0;
};

#define INVALID_TYPE_ID ((u32)0-1)
template<typename T> constexpr TypeId type_id();

void type_set_name( TypeInfo& type, const char* name );
void type_set_size( TypeInfo& type, const u32 size );
void type_set_id( TypeInfo& type, const TypeId type_id );

struct TypeInfo
{
    TypeInfo( TypeInfo_Type _type );

    std::string name;
    u32 size;
    TypeInfo_Type type;
    TypeId type_id;
};

enum class ScalarInfo_Type
{
    INT,
    UINT,
    FLOAT,
    CHAR,
    BOOL
};

void scalar_set_type( ScalarInfo& scalar, ScalarInfo_Type type );

struct ScalarInfo : public TypeInfo
{
    static const TypeInfo_Type ti_type = TypeInfo_Type::SCALAR;
    static std::string get_name( u32 _size, ScalarInfo_Type _scalar_type );

    ScalarInfo();

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

struct TemplateInstanceRef
{
    TemplateInstanceRef();
    TemplateInstanceRef( const TemplateInfo* _definition, i32 _inst_idx );
    
    const TemplateInfo* definition;
    i32                 inst_idx;
    const TemplateInstance* operator->() const;
    bool operator==( const TemplateInstanceRef& other ) const;
    bool operator==( const std::nullptr_t other ) const;
    bool operator!=( const std::nullptr_t other ) const;
    operator bool() const;
};

struct FieldInfo
{
    FieldInfo();
    FieldInfo( const TypeInfo* _type, FieldInfo_Modifier _modifier, u32 _offset );
    FieldInfo( const std::string& _name, FieldInfo_Modifier _modifier, u32 _offset );
    FieldInfo( const std::string& _name, const TypeInfo* _type, FieldInfo_Modifier _modifier, u32 _offset );
    FieldInfo( const std::string& _name, const TemplateInstanceRef _template_type, FieldInfo_Modifier _modifier, u32 _offset );

    std::string name;
    const TypeInfo* type;
    TemplateInstanceRef template_type;
    FieldInfo_Modifier modifier;
    u32 offset;

    FieldInfo& operator=( const FieldInfo& other );
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

void func_set_name( FuncInfo& func, const std::string& _name );
void func_set_return_type( FuncInfo& func, const FieldInfo _return_type );
void func_add_parameter( FuncInfo& func, const FieldInfo _parameter );
void func_finalize_free_function( FuncInfo& func );

struct FuncInfo : public TypeInfo
{
    FuncInfo();

    FieldInfo return_type;
    std::vector<FieldInfo> parameters;
};

struct ObjectData
{
    u32 object_id;
};

void struct_set_parent( StructInfo& type, const StructInfo* parent );
void struct_add_field( StructInfo& type, const FieldInfo& field );
void struct_add_function( StructInfo& type, const FuncInfo& function);
void struct_is_object( StructInfo& type, bool is_object );
void struct_set_object_data( StructInfo& type, const ObjectData object_data );

struct StructInfo : public TypeInfo
{
    static const TypeInfo_Type ti_type = TypeInfo_Type::STRUCT;

    StructInfo();

    const FieldInfo& get_field( const std::string& field_name ) const;
    const FuncInfo& get_func( const std::string& functione_name ) const;

    const StructInfo* parent;
    std::vector<FieldInfo> fields;
    std::vector<FuncInfo> functions;
    bool is_object;
    ObjectData object_data;
};

void enum_set_underlying_type( EnumInfo& type, const TypeInfo* underlying_type );
void enum_add_value( EnumInfo& type, const std::string& name, i64 value );

struct EnumInfo : public TypeInfo
{
    static const TypeInfo_Type ti_type = TypeInfo_Type::ENUM;

    EnumInfo();

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

    const TemplateInfo*                definition;
    const std::array<TemplateParam, 4> params;
};

struct TemplateInfo : public TypeInfo
{
    static const TypeInfo_Type ti_type = TypeInfo_Type::TEMPLATE;

    TemplateInfo();
    std::vector<TemplateInstance> instances;

    bool                has_instance( const std::array<TemplateParam, 4>& params );
    TemplateInstanceRef get_instance( const std::array<TemplateParam, 4>& params, bool create_if_needed = false );

private:
    i32 get_instance_internal( const std::array<TemplateParam, 4>& params );
};