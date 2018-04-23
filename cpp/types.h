#ifndef TYPES_H
#define TYPES_H

#include <cstdint>

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

struct TypeId
{
    uint32_t global_type = 0;
    uint32_t local_type  = 0;
};

#define INVALID_TYPE_ID ((uint32_t)0-1)
template<typename T> constexpr TypeId type_id();

enum class ScalarInfoType
{
    INT,
    UINT,
    FLOAT,
    CHAR,
    BOOL
};

struct ScalarInfo
{
    uint32_t        size = 0;
    ScalarInfoType  scalar_type = ScalarInfoType::INT;
};

struct TemplateInstanceRef
{
    const TemplateInfo* definition = nullptr;
    int32_t             inst_idx   = -1;

    const TemplateInstance* operator->() const;
    bool operator==( const TemplateInstanceRef& other ) const;
    bool operator==( const std::nullptr_t other ) const;
    bool operator!=( const std::nullptr_t other ) const;
    operator bool() const;
};

enum FieldInfoModifier
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
    const char* name     = nullptr;
    const TypeInfo* type = nullptr;
    TemplateInstanceRef template_type = {};
    FieldInfoModifier modifier = NONE;
    uint32_t offset = 0;

    FieldInfo& operator=( const FieldInfo& other );
    bool operator==( const FieldInfo& other ) const;
    operator bool() const;
};

struct FuncInfo
{
    FieldInfo return_type = {};

    FieldInfo* parameters        = nullptr;
    unsigned int parameter_count = 0;
};

struct ObjectData
{
    uint32_t object_id = INVALID_TYPE_ID;
};

struct StructInfo
{
    const FieldInfo* get_field( const char* field_name ) const;
    const FuncInfo*  get_func( const char* func_name ) const;

    const TypeInfo* parent = nullptr;
    uint32_t        size   = 0;

    FieldInfo*   fields = nullptr;
    unsigned int field_count = 0;

    FuncInfo* functions = nullptr;
    unsigned int function_count = 0;
    
    bool is_object = false;
    ObjectData object_data = {};
};

struct EnumValue
{
    const char* name;
    int64_t     value;
};

struct EnumInfo
{
    EnumValue*   enum_values      = nullptr;
    uint32_t      enum_value_count = 0;
    const TypeInfo* underlying_type;
};

struct TemplateParam
{
    FieldInfo info;
    int32_t   instance_value; // depends on type_info
};

struct TemplateInstance
{
    const TemplateInfo*                definition;

    TemplateParam* params;
    uint32_t       param_count;
};

struct TemplateInfo
{
    uint32_t          instance_count;
    TemplateInstance* instances;

    bool                has_instance( const TemplateParam* params, uint32_t param_count );
    TemplateInstanceRef get_instance( const TemplateParam* params, uint32_t param_count, bool create_if_needed = false );

private:
    int32_t get_instance_internal( const TemplateParam* params, unsigned int param_count );
};

enum class TypeInfoType
{
    Scalar,
    Struct,
    Enum,
    Template,
    Function,
};

struct TypeInfo
{
    const char*  name;
    TypeInfoType type;
    TypeId       type_id;

    union
    {
        TemplateInfo template_info;
        ScalarInfo   scalar_info;
        StructInfo   struct_info;
        FuncInfo     func_info;
        EnumInfo     enum_info;
    };

    TypeInfo& operator=( const TypeInfo& other );
};

void scalar_set_type( ScalarInfo& type, ScalarInfoType scalar_type );
void scalar_set_size( ScalarInfo& type, uint32_t size );

void func_set_name( FuncInfo& type, const char* _name );
void func_set_return_type( FuncInfo& type, const FieldInfo _return_type );
void func_add_parameter( FuncInfo& type, const FieldInfo _parameter );
void func_set_parameters( FuncInfo& type, FieldInfo* fields, uint32_t count );

void struct_set_size( StructInfo& type, uint32_t size );
void struct_set_parent( StructInfo& type, const TypeInfo* parent );
void struct_set_fields( StructInfo& type, FieldInfo* fields, uint32_t count );
void struct_set_functions( StructInfo& type, FuncInfo* functions, uint32_t count );
void struct_is_object( StructInfo& type, bool is_object );
void struct_set_object_data( StructInfo& type, const ObjectData object_data );

void enum_set_underlying_type( EnumInfo& type, const TypeInfo* underlying_type );
void enum_set_values( EnumInfo& type, EnumValue* values, uint32_t count );

void template_set_instances( TemplateInfo& type, TemplateInstance* instances, uint32_t count );

void type_set_name( TypeInfo& type, const char* name );
void type_set_type( TypeInfo& type, TypeInfoType type_type );
void type_set_id( TypeInfo& type, TypeId type_id );

#endif

#ifdef TYPES_IMPLEMENTATION

void scalar_set_type( ScalarInfo& type, ScalarInfoType scalar_type )
{
    type.scalar_type = scalar_type;
}
void scalar_set_size( ScalarInfo& type, uint32_t size )
{
    type.size = size;
}

void func_set_return_type( FuncInfo& type, const FieldInfo return_type )
{
    type.return_type = return_type;
}
void func_set_parameters( FuncInfo& type, FieldInfo* params, uint32_t count )
{
    type.parameters      = params;
    type.parameter_count = count;
}

void struct_set_size( StructInfo& type, uint32_t size )
{
    type.size = size;
}
void struct_set_parent( StructInfo& type, const TypeInfo* parent )
{
    type.parent = parent;
}
void struct_set_fields( StructInfo& type, FieldInfo* fields, uint32_t count )
{
    type.fields      = fields;
    type.field_count = count;
}
void struct_set_functions( StructInfo& type, FuncInfo* functions, uint32_t count )
{
    type.functions      = functions;
    type.function_count = count;
}
void struct_is_object( StructInfo& type, bool is_object )
{
    type.is_object = is_object;
}
void struct_set_object_data( StructInfo& type, const ObjectData object_data );

void enum_set_underlying_type( EnumInfo& type, const TypeInfo* underlying_type )
{
    type.underlying_type = underlying_type;
}
void enum_set_values( EnumInfo& type, EnumValue* values, uint32_t count ) 
{ 
    type.enum_values      = values; 
    type.enum_value_count = count; 
}

void template_set_instances( TemplateInfo& type, TemplateInstance* instances, uint32_t count )
{
    type.instances      = instances;
    type.instance_count = count;
}

void type_set_name( TypeInfo& type, const char* name )
{ 
    type.name = name; 
}
void type_set_type( TypeInfo& type, TypeInfoType type_type )
{
    type.type = type_type;
    switch( type_type )
    {
        case TypeInfoType::Enum:
            type.enum_info = {};
            break;
        case TypeInfoType::Function:
            type.func_info = {};
            break;
        case TypeInfoType::Scalar:
            type.scalar_info = {};
            break;
        case TypeInfoType::Struct:
            type.struct_info = {};
            break;
        case TypeInfoType::Template:
            type.template_info = {};
            break;
    }
}

void type_set_id( TypeInfo& type, TypeId type_id )
{
    type.type_id = type_id;
}

TypeInfo& TypeInfo::operator=( const TypeInfo& other )
{
    memcpy( this, &other, sizeof(TypeInfo) );
}

FieldInfo& FieldInfo::operator=( const FieldInfo& other )
{
    name          = other.name;
    type          = other.type;
    template_type = other.template_type;
    modifier      = other.modifier;
    offset        = other.offset;
    return *this;
}

bool FieldInfo::operator==( const FieldInfo& other ) const
{
    return ( this->type == other.type && this->template_type == other.template_type ) 
           && this->modifier == other.modifier 
           && this->offset == other.offset;
}

FieldInfo::operator bool() const
{
    return this->type != nullptr || this->template_type != nullptr;
}


int32_t TemplateInfo::get_instance_internal( const TemplateParam* params, uint32_t param_count )
{
    for(uint32_t i=0; i<instance_count; ++i)
    {
        const auto& inst = instances[i];
        bool all_param_match = true;
        for(uint32_t j=0; j<inst.param_count; ++j)
        {
            const auto& inst_param = inst.params[j];
            const auto& comp_param = params[j];
            if( !inst_param.info || !comp_param.info )
            {
                if( inst_param.info || comp_param.info)
                    all_param_match = false;
                break;
            }

            if( inst_param.info == comp_param.info )
            {
                if( inst_param.info.type->type == TypeInfoType::Scalar )
                {
                    if( inst_param.instance_value != comp_param.instance_value )
                    {
                        all_param_match = false;
                        break;
                    }
                }
            }
            else
            {
                all_param_match = false;
                break;
            }
        }
        if (all_param_match)
            return i;
    }
    return -1;
}

bool TemplateInfo::has_instance( const TemplateParam* params, uint32_t param_count )
{
    return get_instance_internal( params, param_count ) >= 0;
}

TemplateInstanceRef TemplateInfo::get_instance( const TemplateParam* params, uint32_t param_count, bool create_if_needed )
{
    return TemplateInstanceRef{ this, get_instance_internal( params, param_count ) };
}

const TemplateInstance* TemplateInstanceRef::operator->() const { return definition == nullptr ? nullptr : &definition->instances[inst_idx]; }

bool TemplateInstanceRef::operator==( const TemplateInstanceRef& other ) const { return definition == other.definition && inst_idx == other.inst_idx; }
bool TemplateInstanceRef::operator==( const std::nullptr_t other ) const { return definition == nullptr; }
bool TemplateInstanceRef::operator!=( const std::nullptr_t other ) const { return definition != nullptr; }
TemplateInstanceRef::operator bool() const { return *this != nullptr; }

#endif