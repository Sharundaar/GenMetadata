#include "types.h"

#include "object.h"

/* TypeInfo */

void type_set_name( TypeInfo& type, const char* name ) { type.name = name; }
void type_set_size( TypeInfo& type, const u32 size ) { type.size = size; }
void type_set_id( TypeInfo& type, const TypeId type_id ) { type.type_id = type_id; }

TypeInfo::TypeInfo( TypeInfo_Type _type )
    : name(), size(), type( _type ), type_id()
{}




/* ScalarInfo */

void scalar_set_type( ScalarInfo& scalar, ScalarInfo_Type type )
{
    scalar.scalar_type = type;
    auto name = ScalarInfo::get_name( scalar.size, scalar.scalar_type );
    type_set_name( scalar, name.c_str() );
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
    case ScalarInfo_Type::BOOL:
        return "bool";
        break;
    case ScalarInfo_Type::CHAR:
        return "char";
        break;
    default:
        break;
    }

    return ""; // should not reach here
}

ScalarInfo::ScalarInfo()
    : TypeInfo( TypeInfo_Type::SCALAR ), scalar_type()
{
}




/* FieldInfo */

FieldInfo::FieldInfo()
    : name(), type(nullptr), template_type(), modifier(NONE), offset(0)
{}

FieldInfo::FieldInfo( const std::string& _name, FieldInfo_Modifier _modifier, u32 _offset )
    : name(_name), type(nullptr), template_type(), modifier(_modifier), offset(_offset)
{}

FieldInfo::FieldInfo( const TypeInfo* _type, FieldInfo_Modifier _modifier, u32 _offset )
    : name(), type(_type), template_type(), modifier( _modifier ), offset( _offset )
{
}

FieldInfo::FieldInfo( const std::string& _name, const TypeInfo* _type, FieldInfo_Modifier _modifier, u32 _offset )
    : name(_name), type(_type), template_type(), modifier( _modifier ), offset( _offset )
{
}

FieldInfo::FieldInfo( const std::string& _name, const TemplateInstanceRef _template_type, FieldInfo_Modifier _modifier, u32 _offset )
    : name(_name), type(nullptr), template_type(_template_type), modifier((FieldInfo_Modifier)(FieldInfo_Modifier::TEMPLATE | _modifier)), offset(_offset)
{
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




/* FuncInfo */

void func_set_return_type( FuncInfo& func, const FieldInfo _return_type ) { func.return_type = _return_type; }
void func_add_parameter( FuncInfo& func, const FieldInfo _parameter ) { func.parameters.emplace_back( _parameter ); }

FuncInfo::FuncInfo()
    : TypeInfo( TypeInfo_Type::FUNCTION ), return_type(), parameters()
{}


/* EnumInfo */

void enum_add_value( EnumInfo& type, const std::string& name, i64 value ) { type.enum_values.emplace( name, value ); }
void enum_set_underlying_type( EnumInfo& type, const TypeInfo* underlying_type ) { type.underlying_type = underlying_type; }

EnumInfo::EnumInfo()
    : TypeInfo( TypeInfo_Type::ENUM ), enum_values(), underlying_type()
{
}




/* StructInfo */

static u32 generate_object_id()
{
    static u32 s_high_type_id = 0;
    return s_high_type_id++;
}

static ObjectData from_struct_type( const StructInfo* struct_type )
{
    if( !struct_type->is_object )
        return ObjectData{ INVALID_TYPE_ID };
    ObjectData data = ObjectData { generate_object_id() };
    return data;
}

void struct_set_parent( StructInfo& type, const StructInfo* parent )
{
    type.parent = parent;
    // type.is_object = is_object_func( &type );
    type.object_data = from_struct_type( &type );
}
void struct_add_field( StructInfo& type, const FieldInfo& field ) { type.fields.emplace_back( field ); }
void struct_add_function( StructInfo& type, const FuncInfo& function) { type.functions.emplace_back( function ); }
void struct_is_object( StructInfo& type, bool is_object ) { type.is_object = is_object; }
void struct_set_object_data( StructInfo& type, const ObjectData object_data ) { type.object_data = object_data; }

StructInfo::StructInfo()
    : TypeInfo( TypeInfo_Type::STRUCT ), parent(nullptr), fields(), functions(), is_object( false ), object_data()
{
}

static FieldInfo errorField;
const FieldInfo& StructInfo::get_field( const std::string& field_name ) const
{
    for(const auto& field : fields )
    {
        if( field.name == field_name )
        {
            return field;
        }
    }

    // @Error: should err here...
    return errorField;
}

/* TempalteInfo */

TemplateInfo::TemplateInfo()
    : TypeInfo( TypeInfo_Type::TEMPLATE ), instances()
{
}

i32 TemplateInfo::get_instance_internal( const std::array<TemplateParam, 4>& params )
{
    for(u32 i=0; i<instances.size(); ++i)
    {
        const auto& inst = instances[i];
        bool all_param_match = true;
        for(u32 j=0; j<inst.params.size(); ++j)
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
                if( inst_param.info.type->type == TypeInfo_Type::SCALAR )
                {
                    if( inst_param.instance_value.int_64 != comp_param.instance_value.int_64 )
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

bool TemplateInfo::has_instance( const std::array<TemplateParam, 4>& params )
{
    return get_instance_internal( params ) >= 0;
}

TemplateInstanceRef TemplateInfo::get_instance( const std::array<TemplateParam, 4>& params, bool create_if_needed )
{
    auto idx = get_instance_internal( params );
    if( idx < 0 && create_if_needed )
    {
        idx = instances.size();
        instances.push_back( TemplateInstance( this, params ) );
    }
    return TemplateInstanceRef{ this, idx };
}

TemplateInstance::TemplateInstance( const TemplateInfo* _definition, const std::array<TemplateParam, 4>& _params )
    : definition(_definition), params(_params)
{
}

const TemplateInstance* TemplateInstanceRef::operator->() const { return definition == nullptr ? nullptr : &definition->instances[inst_idx]; }

TemplateInstanceRef::TemplateInstanceRef()
    : definition(nullptr), inst_idx(0)
{}
TemplateInstanceRef::TemplateInstanceRef( const TemplateInfo* _definition, i32 _inst_idx )
    : definition(_definition), inst_idx(_inst_idx)
{}

bool TemplateInstanceRef::operator==( const TemplateInstanceRef& other ) const { return definition == other.definition && inst_idx == other.inst_idx; }
bool TemplateInstanceRef::operator==( const std::nullptr_t other ) const { return definition == nullptr; }
bool TemplateInstanceRef::operator!=( const std::nullptr_t other ) const { return definition != nullptr; }
TemplateInstanceRef::operator bool() const { return *this != nullptr; }