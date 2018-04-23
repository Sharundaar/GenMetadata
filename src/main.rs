extern crate argparse;
extern crate clang;

use std::path::PathBuf;
use std::fmt;
use std::fmt::Display;
use std::fs::read_dir;
use std::time::Instant;
use argparse::{ArgumentParser, StoreTrue, StoreOption, Collect, List};
use clang::*;
use std::result::Result;
use std::fs::File;
use std::io::Write;
use std::collections::HashSet;
use std::collections::HashMap;

#[derive(Default)]
struct Options {
    verbose: bool,
    input_directories: Vec<String>,
    additional_include_directories: Vec<String>,
    output_file: Option<String>,
    no_output: bool,
    no_report: bool,
}

#[derive(Clone, Default)]
struct TypeInfo {
    name: String,
    source_file: Option<String>,
    built_in: bool,
    _struct:     Option<TypeInfoStruct>,
    _field:      Option<TypeInfoField>,
    _scalar:     Option<TypeInfoScalar>,
    _enum:       Option<TypeInfoEnum>,
    _func:       Option<TypeInfoFunc>,
    _template:   Option<TypeInfoTemplate>,
    _typedef:    Option<TypeInfoTypedef>,
}

#[derive(Clone, Default)]
struct TypeInfoTemplate {

}

#[derive(Clone, Default)]
struct TypeInfoTypedef {
    source_type: String
}

#[derive(Clone)]
enum TypeInfoStructKind
{
    Struct,
    Class,
}

#[derive(Clone, Default)]
struct TypeInfoStruct {
    fields: Vec<TypeInfo>,
    functions: Vec<TypeInfo>,
    parent: Option<String>,
    kind: TypeInfoStructKind,
    has_default_constructor: bool,
}

#[derive(Clone, Default)]
struct TypeInfoField {
    field_name: String,
    templates: Option<Vec<TypeInfo>>,
    offset: u32,
    is_const: bool,
    is_private: bool,
    is_ptr: bool,
    is_ref: bool,
}

#[derive(Clone)]
enum ScalarInfo {
    INT,
    UINT,
    FLOAT,
    BOOL,
    CHAR
}

#[derive(Clone, Default)]
struct TypeInfoScalar {
    scalar_type: ScalarInfo
}

#[derive(Clone, Default)]
struct TypeInfoEnum {
    underlying_type: String,
    enum_values: HashMap<String, (i64, u64)>,
}

#[derive(Clone, Default)]
struct TypeInfoFunc {
    return_type: Option<Box<TypeInfo>>,
    parameters:  Vec<TypeInfo>,
}

enum GMErrorKind {
    GENERIC,
    IO,
}

#[allow(dead_code)]
enum GMErrorGravity {
    INFO,
    WARNING,
    ERROR,
}

#[allow(dead_code)]
struct GMError {
    gravity: GMErrorGravity,
    kind: GMErrorKind,
    description: String,
}

enum TypeInfoType<'a> {
    None,
    Struct( &'a TypeInfoStruct ),
    Field( &'a TypeInfoField ),
    Scalar( &'a TypeInfoScalar ),
    Enum( &'a TypeInfoEnum ),
    Func( &'a TypeInfoFunc ),
    Template( &'a TypeInfoTemplate ),
    Typedef( &'a TypeInfoTypedef ),
}

fn get_type_info_type<'a>( type_info: &'a TypeInfo ) -> TypeInfoType<'a> {
    use TypeInfoType::*;
    if      let Some( ref sub_type ) = type_info._struct { Struct( sub_type ) }
    else if let Some( ref sub_type ) = type_info._enum { Enum( sub_type ) }
    else if let Some( ref sub_type ) = type_info._scalar { Scalar( sub_type ) }
    else if let Some( ref sub_type ) = type_info._template { Template( sub_type ) }
    else if let Some( ref sub_type ) = type_info._field { Field( sub_type ) }
    else if let Some( ref sub_type ) = type_info._func { Func( sub_type ) }
    else if let Some( ref sub_type ) = type_info._typedef { Typedef( sub_type ) }
    else { None }
}

impl GMError {
    #[allow(dead_code)]
    fn new( gravity: GMErrorGravity, kind: GMErrorKind, description: String ) -> GMError {
        GMError{ gravity: gravity, kind: kind, description: description }
    }

    #[allow(dead_code)]
    fn warning( description: String ) -> GMError {
        GMError{ gravity: GMErrorGravity::WARNING, kind: GMErrorKind::GENERIC, description: description }
    }

    fn error( description: String ) -> GMError {
        GMError{ gravity: GMErrorGravity::ERROR, kind: GMErrorKind::GENERIC, description: description }
    }

    fn info( description: String ) -> GMError {
        GMError{ gravity: GMErrorGravity::INFO, kind: GMErrorKind::GENERIC, description: description }
    }
}

impl From<std::io::Error> for GMError {
    fn from( e: std::io::Error ) -> GMError {
        use std::error::Error;
        GMError{ gravity: GMErrorGravity::ERROR, kind: GMErrorKind::IO, description: e.description().to_string() }
    }
}

impl Display for GMError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // use GMErrorKind::*;
        use GMErrorGravity::*;

        match self.gravity {
            INFO => write!(f, "INFO: "),
            WARNING => write!(f, "WARNING: "),
            ERROR => write!(f, "ERROR: "),
        }?;

        write!(f, "{}", self.description)
    }
}

impl Default for ScalarInfo {
    fn default() -> ScalarInfo { ScalarInfo::INT }
}

impl Default for TypeInfoStructKind {
    fn default() -> TypeInfoStructKind { TypeInfoStructKind::Struct }
}

impl Display for ScalarInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ScalarInfo::*;
        match *self {
            INT => write!(f, "int"),
            UINT => write!(f, "uint"),
            FLOAT => write!(f, "float"),
            CHAR => write!(f, "char"),
            BOOL => write!(f, "bool"),
        }
    }
}

impl TypeInfo {
    fn new<S>( name: S ) -> TypeInfo where S: Into<String> {
        TypeInfo{ name: name.into(), ..Default::default() }
    }

    fn make_scalar( mut self, scalar_type: ScalarInfo ) -> TypeInfo {
        self._scalar = Some( TypeInfoScalar { scalar_type: scalar_type } );
        self
    }

    fn make_field( mut self, field_name: &str, offset: u32 ) -> TypeInfo {
        self._field = Some( TypeInfoField{ field_name: String::from( field_name ), offset: offset, ..Default::default() } );
        self
    }

    fn make_builtin( mut self ) -> TypeInfo {
        self.built_in = true;
        self
    }

    fn make_template( mut self ) -> TypeInfo {
        self._template = Some( TypeInfoTemplate::default() );
        self
    }

    fn make_struct( mut self, parent: &Option<&str>, kind: TypeInfoStructKind ) -> TypeInfo {
        self._struct = Some( TypeInfoStruct{ parent: parent.map(|x| x.to_string()), kind: kind, ..Default::default() });
        self
    }

    fn make_enum( mut self, underlying_type: &str ) -> TypeInfo {
        self._enum = Some( TypeInfoEnum{ underlying_type: String::from( underlying_type ), ..Default::default() } );
        self
    }

    fn make_func( mut self, return_type: Option<Box<TypeInfo>> ) -> TypeInfo {
        self._func = Some( TypeInfoFunc{ return_type: return_type, ..Default::default() } );
        self
    }

    fn make_typedef( mut self, source_type: String ) -> TypeInfo {
        self._typedef = Some( TypeInfoTypedef{ source_type: source_type } );
        self
    }
}

fn has_object_parent( struct_info: &TypeInfoStruct, type_info_map: &HashMap<String, &TypeInfo> ) -> bool {
    if let Some( ref parent ) = struct_info.parent {
        if parent == "Object" {
            return true;
        }

        if let Some( ref parent_type ) = type_info_map.get( parent ) {
            if let Some( ref parent_struct ) = parent_type._struct {
                return has_object_parent( &parent_struct, &type_info_map );
            }
        }
    }

    return false;
}

fn get_source_file( entity: &Entity ) -> Option<String> {
    match entity.get_location() {
        Some( location ) => {
            if let Some( file_location ) = location.get_file_location().file {
                Some( file_location.get_path().file_name().unwrap().to_str().unwrap().to_string() )
            }
            else {
                None
            }
        },
        None => None,
    }
}

fn get_struct_kind_from_entity_kind( ent_kind: EntityKind ) -> TypeInfoStructKind {
    match ent_kind {
        EntityKind::ClassDecl => TypeInfoStructKind::Class,
        EntityKind::StructDecl => TypeInfoStructKind::Struct,
        _ => TypeInfoStructKind::Struct,
    }
}

fn from_entity_structdecl( entity: &Entity ) -> Result<TypeInfo, GMError> {
    if !entity.is_definition() { return Err( GMError::info( "not definition".to_string() ) ); }
    match ( entity.get_name(), entity.get_type() ) {
        ( Some( name ), Some( _ ) ) => {
            let mut type_info = TypeInfo::new( name ).make_struct( &None, get_struct_kind_from_entity_kind( entity.get_kind() ) );
            type_info.source_file = get_source_file( entity );

            {
                use EntityKind::*;
                let mut type_info_struct = type_info._struct.as_mut().unwrap();
                for child in entity.get_children().into_iter() {
                    match child.get_kind() {
                        BaseSpecifier => {
                            if let Some( type_def ) = child.get_type() {
                                type_info_struct.parent = Some( type_def.get_display_name().clone() );
                            }
                        },
                        FieldDecl => {
                            match from_entity( &child ) {
                                Ok( type_field ) => { type_info_struct.fields.push( type_field ); },
                                Err( err ) => { println!( "{}", err )},
                            }
                        },
                        Method => {
                            match from_entity( &child ) {
                                Ok( type_function ) => { type_info_struct.functions.push( type_function ); },
                                Err( _ ) => {},
                            }
                        },
                        Constructor => {
                            if child.is_default_constructor() {
                                type_info_struct.has_default_constructor = true;
                            }
                        }
                        // kind => { println!( "Unhandled entity kind in struct: {:?}", kind ); }
                        _ => {}
                    }
                }
            }

            let type_info = type_info;
            Ok( type_info )
        },
        ( None, Some(_) ) => Err( GMError::error( "Couldn't generate a TypeInfo from this StructDecl (missing name).".to_string() ) ),
        ( Some(_), None ) => Err( GMError::error( "Couldn't generate a TypeInfo from this StructDecl (missing type).".to_string() ) ),
        ( None, None ) => Err( GMError::error( "Couldn't generate a TypeInfo from this StructDecl.".to_string() ) ),
    }
}

fn remove_template_args( type_name: String ) -> String {
    type_name.split('<').nth(0).unwrap_or_else( || &type_name ).to_string()
}

fn sanitize_type_name( arg: &Type ) -> String {
    remove_template_args( arg.get_display_name().replace("const", "").trim().to_string() )
}

fn make_field_from_type<'a>( field_info: &mut TypeInfoField, type_def: &'a Type ) -> Result<Type<'a>, GMError> {
    use TypeKind::*;
    let type_def = match type_def.get_kind() {
        Pointer => { 
            field_info.is_ptr = true; 
            let pointee_type = type_def.get_pointee_type().unwrap();
            if pointee_type.get_kind() == Pointer {
                return Err( GMError::error( "Unable to parse double pointer types. (eg. int**, int&* and others)".to_string() ) );
            }
            pointee_type
        },
        LValueReference => { 
            field_info.is_ref = true;
            let pointee_type = type_def.get_pointee_type().unwrap();
            if pointee_type.get_kind() == Pointer {
                return Err( GMError::error( "Unable to parse double ref types. (eg. int*&)".to_string() ) );
            }
            pointee_type
        },
        _ => {
            *type_def
        },
    };
    
    field_info.is_const = type_def.is_const_qualified();

    if let Some( template_args ) = type_def.get_template_argument_types() {
        let mut tas: Vec<TypeInfo> = vec!();
        for arg in template_args.iter().filter_map( |a| *a ) {
            let mut thing = TypeInfo::new( "" ).make_field( "", 0 );
            let true_type = make_field_from_type( thing._field.as_mut().unwrap(), &arg )?;
            thing.name = sanitize_type_name( &true_type );
            tas.push( thing );
        }
        field_info.templates = Some( tas );
    }

    Ok( type_def )
}

fn from_entity_fielddecl( entity: &Entity, parent_type: Option<&Type> ) -> Result<TypeInfo, GMError> {
    match ( entity.get_name(), entity.get_type() ) {
        ( Some( name ), Some( type_def ) ) => {
            // type name filled in later
            let mut type_info = TypeInfo::new( "" ).make_field( &name, if let Some(parent_type) = parent_type { ( parent_type.get_offsetof( &name ).unwrap() as u32 ) / 8 } else { 0 } );
            {
                let mut field_info = type_info._field.as_mut().unwrap();

                if let Some(accessibility) = entity.get_accessibility() {
                    use Accessibility::*;
                    match accessibility {
                        Private | Protected => field_info.is_private = true,
                        Public              => field_info.is_private = false,
                    }
                }

                let type_def = make_field_from_type( &mut field_info, &type_def )?;
                type_info.name = sanitize_type_name( &type_def );
            }

            let type_info = type_info;
            Ok( type_info )
        },
        _ => Err( GMError::error( "Couldn't generate Field from this FieldDecl.".to_string() ) ),
    }
}

fn from_entity_enumdecl( entity: &Entity ) -> Result<TypeInfo, GMError> {
    if !entity.is_definition() { return Err( GMError::info( "not definition".to_string() ) ); }
    match ( entity.is_scoped(), entity.get_type() ) {
        ( true, Some( type_def ) ) => {
            let mut type_info = TypeInfo::new( type_def.get_display_name() ).make_enum( &entity.get_enum_underlying_type().unwrap().get_display_name() );
            type_info.source_file = get_source_file( entity );

            {
                let mut type_enum = type_info._enum.as_mut().unwrap();
                for child in entity.get_children() {
                    type_enum.enum_values.insert( child.get_name().unwrap().clone(), child.get_enum_constant_value().unwrap() );
                }
            }

            let type_info = type_info;
            Ok( type_info )
        },
        ( false, Some( type_def ) ) => Err( GMError::warning( format!("Enum {} is unscoped, can't generate TypeInfo for unscoped enums.", type_def.get_display_name() ) ) ),
        _ => Err( GMError::error( "Couldn't generate TypeInfo from this EnumDecl.".to_string() ) ),
    }
}

fn from_entity_funcdecl( entity: &Entity ) -> Result<TypeInfo, GMError> {
    if let Some( _ ) = entity.get_template() { return Err( GMError::info( "we don't handle template func.".to_string() ) ); }

    match ( entity.get_name(), entity.get_type() ) {
        ( Some( name ), Some( ent_type ) ) => {
            let return_type_type = ent_type.get_result_type().unwrap();
            let mut return_type: Option<Box<TypeInfo>> = None;
            if return_type_type.get_display_name() != "void" {
                let mut return_type_ti = TypeInfo::new( "" ).make_field( "", 0 );
                let return_type_type = make_field_from_type( return_type_ti._field.as_mut().unwrap(), &return_type_type )?;
                return_type_ti.name = sanitize_type_name( &return_type_type );
                return_type = Some( Box::new( return_type_ti ) );
            }
            let mut type_info = TypeInfo::new( name ).make_func( return_type );
            type_info.source_file = get_source_file( entity );
            {
                let mut type_func = type_info._func.as_mut().unwrap();
                for child in entity.get_children().iter().filter( |x| x.get_kind() == EntityKind::ParmDecl ) {
                    match from_entity( child ) {
                        Ok( param_type ) => type_func.parameters.push( param_type ),
                        Err( _ ) => {}
                    };
                }
            }
            Ok( type_info )
        }
        _ => Err( GMError::info( "Missing name or type for func decl.".to_string() ) )
    }
}

fn from_entity_classtemplate( entity: &Entity ) -> Result<TypeInfo, GMError> {
    if !entity.is_definition() {
        return Err( GMError::info( "Not definition.".to_string() ) );
    }

    let name = entity.get_name().unwrap();
    let type_info = TypeInfo::new( name ).make_template();
/* @TODO: if we want more informations about the templates...
    for child in entity.get_children() {
        use EntityKind::*;
        match child.get_kind() {
            TemplateTypeParameter => {
                println!( "{:?}", child.get_type() );
             },
            NonTypeTemplateParameter => {
                println!( "{:?}", child.get_type() );
            },
            kind => {
                println!("Unhandled template child: {:?}", kind);
            }
        };
    }
*/
    Ok( type_info )
}

fn from_entity_typedefdecl( entity: &Entity ) -> Result<TypeInfo, GMError> {
    let underlying_type = entity.get_type().unwrap().get_canonical_type().get_display_name();
    let type_name = entity.get_name().unwrap();

    let built_in_types = get_built_in_types();
    for built_in in built_in_types {
        if built_in.name == underlying_type {
            return Ok( TypeInfo::new( type_name ).make_typedef( underlying_type ) );
        }
    }

    Err( GMError::info( format!( "Found typedef that's not a basic type {} -> {}.", underlying_type, type_name ) ) )
}

fn from_entity( entity: &Entity ) -> Result<TypeInfo, GMError> {
    match entity.get_kind() {
        EntityKind::StructDecl    => from_entity_structdecl( entity ),
        EntityKind::ClassDecl     => from_entity_structdecl( entity ),
        EntityKind::FieldDecl     => from_entity_fielddecl( entity, Some( &entity.get_semantic_parent().unwrap().get_type().unwrap() ) ), // should be ok for a field decl... haven't found a better way to pass this...
        EntityKind::EnumDecl      => from_entity_enumdecl( entity ),
        EntityKind::FunctionDecl  => from_entity_funcdecl( entity ),
        EntityKind::ParmDecl      => from_entity_fielddecl( entity, None ),
        EntityKind::Method        => from_entity_funcdecl( entity ),
        EntityKind::ClassTemplate => from_entity_classtemplate( entity ),
        EntityKind::TypedefDecl   => from_entity_typedefdecl( entity ),
        kind => Err( GMError::info( format!( "Unhandled entity kind: {:?}", kind) ) ),
    }
}

fn write_header( type_info_vec : &Vec<TypeInfo> ) -> Result<bool, GMError> {
    use std::iter::FromIterator;
    let type_info_map: HashMap<String, &TypeInfo> = HashMap::from_iter(type_info_vec.iter().map(|x| (x.name.clone(), x)));

    let mut file = File::create( "type_db.h" )?;

    writeln!( file, "#pragma once" )?;
    writeln!( file, "#include \"types.h\"")?;
    writeln!( file )?;

    writeln!( file, "{}", get_register_types_header( true ) )?;
    writeln!( file )?;

    for type_info in type_info_vec.iter().filter( |t| t._enum.is_some() ) {
        let enum_type = type_info._enum.as_ref().unwrap();
        writeln!( file, "enum class {} : {};", type_info.name, enum_type.underlying_type )?;
    }

    writeln!( file )?;

    for type_info in type_info_vec.iter().filter( |t| t._struct.is_some() && !t.built_in ) {
        match type_info._struct.as_ref().unwrap().kind {
            TypeInfoStructKind::Struct => writeln!( file, "struct {};", type_info.name )?,
            TypeInfoStructKind::Class => writeln!( file, "class {};", type_info.name )?,
        }
    }

    writeln!( file )?;

    writeln!( file, "enum class LocalTypeId : u32")?;
    writeln!( file, "{{")?;
    for type_info in type_info_vec.iter() {
        use TypeInfoType::*;
        match get_type_info_type( &type_info ) {
            Scalar(_)  => { writeln!( file, "    {},", get_type_id( &type_info ) )?; }
            Typedef(_) => { writeln!( file, "    {},", get_type_id( &type_info ) )?; }
            Struct(_)  => { writeln!( file, "    {},", get_type_id( &type_info ) )?; }
            Enum(_)    => { writeln!( file, "    {},", get_type_id( &type_info ) )?; }
            Func(_)    => {}
            Field(_)   => {}
            Template(_) => { writeln!( file, "    {},", get_type_id( &type_info ) )?; }
            None => {}
        };
    }
    writeln!( file, "    COUNT," )?;
    writeln!( file, "}};")?;

    writeln!( file )?;

    writeln!( file, "enum class ObjectTypeId" )?;
    writeln!( file, "{{" )?;
    writeln!( file, "    Object," )?;
    for type_info in type_info_vec.iter()
                    .filter( |t| t._struct.is_some() )
                    .filter( |t| has_object_parent( t._struct.as_ref().unwrap(), &type_info_map ) ) {
        writeln!( file, "    {},", type_info.name )?;
    }
    writeln!( file, "}};" )?;

    writeln!( file )?;

    for type_info in type_info_vec.iter() {
        use TypeInfoType::*;
        match get_type_info_type( &type_info ) {
            Scalar(_)  => { writeln!( file, "template<> constexpr TypeId type_id<{}>() {{ return {{ 0, (u32)LocalTypeId::{} }}; }}", type_info.name, get_type_id(&type_info) )?; }
            Typedef(_) => { continue; }
            Struct(_)  => { writeln!( file, "template<> constexpr TypeId type_id<{}>() {{ return {{ 0, (u32)LocalTypeId::{} }}; }}", type_info.name, get_type_id(&type_info) )?; }
            Enum(_)    => { writeln!( file, "template<> constexpr TypeId type_id<{}>() {{ return {{ 0, (u32)LocalTypeId::{} }}; }}", type_info.name, get_type_id(&type_info) )?; }
            Func(_)    => { continue; }
            Field(_)   => { continue; }
            Template(_) => { continue; }
            None => {}
        };
        writeln!( file, "constexpr TypeId type_id(const {}& obj) {{ return type_id<{}>(); }}", type_info.name, type_info.name )?;
        writeln!( file )?;
    }

    Ok( true )
}

fn build_modifier_string( field: &TypeInfoField ) -> String {
    let mut result: Vec<&str> = vec!();

    if field.is_const   { result.push( "FieldInfo_Modifier::CONSTANT" ); }
    if field.is_ptr     { result.push( "FieldInfo_Modifier::POINTER" ); }
    if field.is_private { result.push( "FieldInfo_Modifier::PRIVATE" ); }
    if field.is_ref     { result.push( "FieldInfo_Modifier::REFERENCE" ); }


    match result.is_empty() {
        true  => String::from( "FieldInfo_Modifier::NONE" ),
        false => result.join( " | " )
    }
}

fn write_field_implementation( type_info_map: &HashMap<String, &TypeInfo>, file: &mut File, field: &TypeInfo, indent: usize ) -> Result<bool, GMError> {
    let indent_str = " ".repeat( indent * 4 );
    if let Some( type_field ) = field._field.as_ref() {
        let field_name = &type_field.field_name;
        if let Some( registered_type ) = type_info_map.get( &field.name ) {
            if registered_type._struct.is_some() {
                writeln!( file, "{indent}FieldInfo( \"{field_name}\", &{field_type}, (FieldInfo_Modifier) ({modifier}), {offset} )", indent = indent_str, field_name = field_name, field_type = get_type_var( &field.name ), modifier = build_modifier_string( &type_field ), offset = type_field.offset )?;
            } else if let Some( ref template_args ) = type_field.templates {
                writeln!( file, "{indent}FieldInfo( \"{field_name}\", type_{template_type}.get_instance( {{", indent = indent_str, field_name = field_name, template_type = field.name.replace("::", "_") )?;
                for arg in template_args {
                    writeln!( file, "{indent}TemplateParam {{ ", indent = indent_str )?;
                    write_field_implementation( type_info_map, file, &arg, indent+1 )?;
                    writeln!( file, "{indent}, {some_value} }},", indent = indent_str, some_value = 0 )?;
                }
                writeln!( file, "{indent}}}, true ), (FieldInfo_Modifier) ({modifier}), {offset} )", indent = indent_str, modifier = build_modifier_string( &type_field ), offset = type_field.offset )?;
            } else {
                writeln!( file, "{indent}FieldInfo( \"{field_name}\", &{field_type}, (FieldInfo_Modifier) ({modifier}), {offset} )", indent = indent_str, field_name = field_name, field_type = get_type_var( &field.name ), modifier = build_modifier_string( &type_field ), offset = type_field.offset )?;
            }
        } else {
            writeln!( file, "{indent}FieldInfo( \"{field_name}\", (FieldInfo_Modifier) ({modifier}), {offset} )", indent = indent_str, field_name = field_name, modifier = build_modifier_string( &type_field ), offset = type_field.offset )?;
        }

        Ok( true )
    } else {
        Err( GMError::error( format!( "Called write_field_implementation without passing a field type as parameter (field type name: {}).", field.name ) ) )
    }
}

fn write_struct_implementation( type_info_map: &HashMap<String, &TypeInfo>, file: &mut File, type_info: &TypeInfo ) -> Result<bool, GMError> {
    let struct_type = type_info._struct.as_ref().unwrap();
    // Write constructors if needed
    if has_object_parent( struct_type, type_info_map ) {
        let parent = struct_type.parent.as_ref().unwrap();
        writeln!( file, "{struct_name}::{struct_name}() : {parent_name}( type_id<{struct_name}>() ) {{}}", struct_name = type_info.name, parent_name = parent )?;
        writeln!( file, "{struct_name}::{struct_name}( TypeId _type_id ) : {parent_name}( _type_id ) {{}}", struct_name = type_info.name, parent_name = parent )?;
    }

    writeln!( file )?;

    Ok( true )
}

fn get_register_types_header( prototype: bool ) -> String {
    let mut result = "void register_types( TypeInfo& (*alloc_type) ( void* ), void* alloc_type_param, void* (*alloc_data) ( void*, uint32_t ), void* alloc_data_param )".to_string();
    if prototype {
        result.push_str( ";" );
    } else {
        result.push_str("\n{");
    }
    result
}

fn get_type_var( type_name: &String ) -> String {
    format!( "type_{}", type_name.replace("::", "_")
                                 .replace("->", "_deref")
                                 .replace(" ", "_")
                                 .replace( "!", "_not" )
                                 .replace( "=", "_equal" )
                                 .replace( ">", "_sup" )
                                 .replace( "<", "_inf" )
                                 .replace("()", "_cast")
                                 .replace("*", "_mul")
                                 .replace("-", "_sub")
                                 .replace("+", "_add")
                                 .replace("/", "_div") )
}

fn get_type_id( type_info: &TypeInfo ) -> String {
    let type_name = &type_info.name;
    if type_info._scalar.is_some() {
        format!( "s{}", type_name.replace("::", "_").replace( " ", "_" ) )
    } else {
        format!( "{}", type_name.replace("::", "_").replace( " ", "_" ) )
    }
}

fn write_type_instantiation( type_info_map: &HashMap<String, &TypeInfo>, file: &mut File, type_info: &TypeInfo, indent_count: usize ) -> Result<bool, GMError> {
    let type_name = &type_info.name;
    let type_var  = get_type_var( &type_name );
    let indent    = " ".repeat( indent_count * 4 );
    
    use TypeInfoType::*;
    match get_type_info_type( &type_info ) {
        Scalar(_) => {
            writeln!( file, "{}auto& {} = alloc_type( alloc_type_param );", indent, type_var )?;
            writeln!( file, "{}type_set_type( {}, TypeInfoType::Scalar );", indent, type_var )?;
        }

        Typedef( typedef ) => {
            let source_type_var = get_type_var( &typedef.source_type );

            writeln!( file, "{}auto& {} = alloc_type( alloc_type_param );", indent, type_var )?;
            writeln!( file, "{}type_set_type( {}, {}.type );", indent, type_var, source_type_var )?;
        }

        Enum(_) => {
            writeln!( file, "{}auto& {} = alloc_type( alloc_type_param );", indent, type_var )?;
            writeln!( file, "{}type_set_type( {}, TypeInfoType::Enum );", indent, type_var )?;
        }

        Template(_) => {
            writeln!( file, "{}auto& {} = alloc_type( alloc_type_param );", indent, type_var )?;
            writeln!( file, "{}type_set_type( {}, TypeInfoType::Template );", indent, type_var )?;
        }

        Struct(_) => {
            writeln!( file, "{}auto& {} = alloc_type( alloc_type_param );", indent, type_var )?;
            writeln!( file, "{}type_set_type( {}, TypeInfoType::Struct );", indent, type_var )?;
        }

        Func(_) => {}

        Field(_) => {}

        None => {}
    };

    Ok(true)
}

fn write_type_implementation( type_info_map: &HashMap<String, &TypeInfo>, file: &mut File, type_info: &TypeInfo, local_instantitation: bool, indent_count: usize ) -> Result<bool, GMError> {
    let type_name = type_info.name.replace( "::", "_" );
    let type_var  = get_type_var( &type_name );
    let indent    = " ".repeat( indent_count * 4 );

    fn open_braces( file: &mut File, indent: &str ) -> Result<(), std::io::Error> {
        writeln!( file, "{}{{", indent )
    }

    fn close_braces( file: &mut File, indent: &str ) -> Result<(), std::io::Error> {
        writeln!( file, "{}}}", indent )
    }

    use TypeInfoType::*;
    match get_type_info_type( &type_info ) {

        Scalar( scalar_type ) => {
            use ScalarInfo::*;
            let scalar_type_name = match scalar_type.scalar_type {
                INT => "INT",
                UINT => "UINT",
                FLOAT => "FLOAT",
                BOOL => "BOOL",
                CHAR => "CHAR",
            };

            writeln!( file, "{}type_set_name( {}, copy_string( \"{}\" ) );", indent, type_var, type_name )?;
            writeln!( file, "{}type_set_id( {}, type_id<{}>() );", indent, type_var, type_info.name )?;
            writeln!( file, "{}scalar_set_size( {}.scalar_info, sizeof( {} ) );", indent, type_var, type_info.name )?;
            writeln!( file, "{}scalar_set_type( {}.scalar_info, ScalarInfoType::{} );", indent, type_var, scalar_type_name )?;
            writeln!( file )?;
        }

        Enum( _enum_type ) => {
            let enum_type = type_info._enum.as_ref().unwrap();
            writeln!( file, "{}type_set_name( {}, copy_string( \"{}\" ) );", indent, type_var, type_name )?;
            writeln!( file, "{}type_set_id( {}, type_id<{}>() );", indent, type_var, type_info.name )?;
            writeln!( file, "{}enum_set_underlying_type( {}.enum_info, &type_{} );", indent, type_var, enum_type.underlying_type.replace("int", "i32").replace("ushort", "u16") )?;
            open_braces( file, &indent )?;
            writeln!( file, "{}    EnumValue* values = (EnumValue*)alloc_data( alloc_data_param, {} );", indent, enum_type.enum_values.len() )?;
            let mut val = 0;
            for ( name, &(value, _) ) in &enum_type.enum_values {
                writeln!( file, "{}    values[{}] = {{ copy_string( \"{}\" ), {} }};", indent, val, name, value )?;
                val = val + 1;
            }
            writeln!( file, "{}    enum_set_values( {}.enum_info, values, {} );", indent, type_var, enum_type.enum_values.len() )?;
            close_braces( file, &indent )?;

            writeln!( file )?;
        }

        Template( _template_type ) => {
            writeln!( file, "{}type_set_name( {}, \"{}\" );", indent, type_var, type_name )?;
            writeln!( file, "{}type_set_id( {}, {{ 0, (u32)LocalTypeId::{} }} );", indent, type_var, get_type_id( &type_info ) )?;
            writeln!( file )?;
        }

        Struct( struct_type ) => {
            writeln!( file, "{}type_set_name( {}, \"{}\" );", indent, type_var, type_name )?;
            writeln!( file, "{}type_set_id( {}, type_id<{}>() );", indent, type_var, type_info.name )?;
            writeln!( file, "{}type_set_size( {}, sizeof( {} ) );", indent, type_var, type_info.name )?;
            if let Some( ref parent ) = struct_type.parent {
                writeln!( file, "{}struct_set_parent( {}, &type_{} );", indent, type_var, parent.replace("::", "_") )?;
            }

            for field in &struct_type.fields {
                writeln!( file, "{}struct_add_field( {}, ", indent, type_var )?;
                write_field_implementation(type_info_map, file, field, indent_count+1)?;
                writeln!( file, "{});", indent)?;
            }

            for func in &struct_type.functions {
                writeln!( file, "{}{{", indent )?;
                write_type_implementation( type_info_map, file, func, true, indent_count+1 )?;
                writeln!( file, "{}    struct_add_function( {}, {} );", indent, type_var, get_type_var( &func.name ) )?;
                writeln!( file, "{}}}", indent )?;
            }
            writeln!( file )?;
        }

        Func( func_type ) => {
            if !local_instantitation {writeln!( file, "{}{{", indent )?;}
            let type_func = func_type;
            let func_name = &type_info.name;

            if local_instantitation {
                writeln!( file, "{}FuncInfo {};", indent, type_var )?;
            } else {
                writeln!( file, "{}auto& {} = static_cast<FuncInfo&>( alloc_type( TypeInfo_Type::FUNCTION, alloc_type_param ) );", indent, type_var )?;
            }

            writeln!( file, "{}type_set_name( {}, \"{}\" );", indent, type_var, func_name )?;
            if let Some( ref return_type ) = type_func.return_type {
                let return_type = &return_type;
                writeln!( file, "{}func_set_return_type( {},", indent, type_var )?;
                write_field_implementation( &type_info_map, file, return_type, indent_count+1 )?;
                writeln!( file, "{});", indent )?;
            }

            for param in &type_func.parameters {
                writeln!( file, "{}func_add_parameter( {}, ", indent, type_var)?;
                write_field_implementation( &type_info_map, file, param, indent_count+1 )?;
                writeln!( file, "{});", indent )?;
            }
            if !local_instantitation {writeln!( file, "{}}}", indent )?;}
            writeln!( file )?;
        }

        Typedef( typedef_type ) => {
            let source_type_var = get_type_var( &typedef_type.source_type );
            writeln!( file, "{}{} = {};", indent, type_var, source_type_var )?;
            writeln!( file, "{}type_set_name( {}, \"{}\" );", indent, type_var, type_name )?;
            writeln!( file, "{}type_set_id( {}, {{ 0, (u32)LocalTypeId::{} }} );", indent, type_var, get_type_id( &type_info ) )?;
            writeln!( file )?;
        }

        Field( _field_type ) => {}

        None => {}
    };

    Ok(true)
}

fn write_implementation( type_info_vec: &Vec<TypeInfo> ) -> Result<bool, GMError> {
    let mut file = File::create( "type_db.cpp" )?;

    // includes
    {
        let mut includes: HashSet<&str> = HashSet::new();

        for type_info in type_info_vec.iter() {
            if let Some( ref source_file ) = type_info.source_file {
                includes.insert( source_file );
            }
        }

        let dependencies = &[ "type_db.h", "basic_types.h" ];
        for dep in dependencies {
            writeln!( file, "#include \"{}\"", dep )?;
            includes.remove( dep );
        }
        includes.remove( "types.h" );

        writeln!( file )?;

        for include in includes.iter() {
            writeln!( file, "#include \"{}\"", include )?;
        }
        writeln!( file )?;
    }

    writeln!( &mut file, "{}", get_register_types_header( false ) )?;

    writeln!( file, "    auto copy_string = [&]( const char* str ) {{
        uint32_t len = strlen( str );
        char* result = (char*)alloc_data( alloc_data_param, len+1 );
        strcpy( result, str );
        return result;
    }};")?;

    use std::iter::FromIterator;
    let type_info_map: HashMap<String, &TypeInfo> = HashMap::from_iter(type_info_vec.iter().map(|x| (x.name.clone(), x)));

    for type_info in type_info_vec.iter() {
        write_type_instantiation( &type_info_map, &mut file, type_info, 1 )?;
    }

    writeln!( file )?;

    for type_info in type_info_vec.iter() {
        write_type_implementation( &type_info_map, &mut file, type_info, false, 1 )?;
    }

    writeln!( file, "}}" )?;

    writeln!( file )?;

    for type_info in type_info_vec.iter().filter( |t| t._struct.is_some() && has_object_parent( t._struct.as_ref().unwrap(), &type_info_map ) ) {
        write_struct_implementation( &type_info_map, &mut file, type_info )?;
    }

    Ok( true )
}


fn generate_main_file( file_list: &Vec<PathBuf>) -> Result<(), String> {
    let mut file = match File::create( "main.h" ) {
        Ok( file ) => file,
        Err( err ) => return Err( err.to_string() ),
    };

    let type_db_file = Some( std::ffi::OsStr::new("type_db.h") );
    for ref file_name in file_list.iter().filter( |n| n.file_name() != type_db_file ) {
        match writeln!( file, "#include \"{}\"", file_name.file_name().unwrap().to_string_lossy() ) {
            Ok(_) => {},
            Err( err ) => return Err( err.to_string() ),
        }
    }

    Ok(())
}

fn get_built_in_types() -> Vec<TypeInfo> {
    use ScalarInfo::*;
    let built_ins: Vec<TypeInfo> = vec![
        TypeInfo::new("bool").make_scalar(BOOL),
        TypeInfo::new("char").make_scalar(CHAR),
        TypeInfo::new("int").make_scalar(INT),
        TypeInfo::new("short").make_scalar(INT),
        TypeInfo::new("long").make_scalar(INT),
        TypeInfo::new("long long").make_scalar(INT),
        TypeInfo::new("unsigned int").make_scalar(UINT),
        TypeInfo::new("unsigned short").make_scalar(UINT),
        TypeInfo::new("unsigned long").make_scalar(UINT),
        TypeInfo::new("unsigned long long").make_scalar(UINT),
        TypeInfo::new("unsigned char").make_scalar(CHAR),
        TypeInfo::new("signed char").make_scalar(CHAR),
        TypeInfo::new("float").make_scalar(FLOAT),
        TypeInfo::new("double").make_scalar(FLOAT),
        TypeInfo::new("std::vector").make_template(),
        TypeInfo::new("std::string").make_struct( &None, TypeInfoStructKind::Class ).make_builtin(),
    ];

    return built_ins;
}

fn get_clang_arguments( input_directories: &Vec<String>, additional_include_directories: &Vec<String> ) -> Vec<String> {
    let mut arguments: Vec<String> = vec![ "-x".into(), "c++".into() ];

    for ref directory in input_directories.iter().chain( additional_include_directories.iter() ) {
        arguments.push( format!( "-I{}", directory ) );
    }

    println!("{:?}", arguments);

    arguments
}

fn show_report_types( type_info_vec: &Vec<TypeInfo> ) {
    use TypeInfoType::*;
    for type_info in type_info_vec.iter() {
        match get_type_info_type( type_info ) {
            Struct( type_struct) => {
                print!("Type: {}", type_info.name);

                if let Some( ref parent ) = type_struct.parent {
                    println!(" (parent: {})", parent);
                } else {
                    println!();
                }

                if type_struct.has_default_constructor {
                    println!("    has_default_constructor: true");
                } else {
                    println!("    has_default_constructor: false");
                }
                for field in &type_struct.fields {
                    println!("    {}: {} ({})", field._field.as_ref().unwrap().offset, field._field.as_ref().unwrap().field_name, field.name);
                }
                for function in &type_struct.functions {
                    let _func_type = function._func.as_ref().unwrap();
                    /* @TODO: Report functions
                    println!("    {} {}({})", func_type.return_type.as_ref().unwrap_or(&"void".to_string()),
                                            function.name, 
                                            func_type.parameters.iter().map( |p| format!("{} {}", p.name, p._field.as_ref().unwrap().field_name ) )
                                                                        .collect::<Vec<String>>().join(", ")); */
                }
            }

            Enum(type_enum) => {
                println!("Enum: {} ({})", type_info.name, type_enum.underlying_type);

                for (name, &(_, uval)) in &type_enum.enum_values {
                    println!("    {}: {}", name, uval);
                }
            }

            Scalar( type_scalar ) => {
                print!("Type: {}", type_info.name);
                println!( " ({})", type_scalar.scalar_type );
            }

            Func( _type_func ) => {
                /* @TODO: Report functions
                let func_type = type_info._func.as_ref().unwrap();
                print!( "Func: {} {} (", func_type.return_type.as_ref().unwrap_or(&"void".to_string()), type_info.name );
                let mut counter = 0;
                for param in func_type.parameters.iter() {
                    let param_type = param._field.as_ref().unwrap();
                    if param_type.is_const {
                        print!("const ");
                    }
                    print!( "{}", param.name );
                    if param_type.is_ptr {
                        print!("*");
                    }
                    print!(" {}", param_type.field_name);

                    counter = counter + 1;
                    if counter < func_type.parameters.len() {
                        print!( ", " );
                    }
                }
                println!(")"); */
            }

            Template(_type_template) => {
                println!("Template: {}", type_info.name);
            }

            Field( _type_field ) => {
                println!("ERROR (for now...): Should not encounter field.");
            }

            Typedef( type_typedef ) => {
                println!("Typedef: {} -> {}", type_typedef.source_type, type_info.name);
            }
            None => { println!("ERROR: get_type_info_type should not return None...");}
        }
    }
}

fn main() {
    let start = Instant::now();

    let mut options : Options = Options::default();

    {
        let mut ap = ArgumentParser::new();
        ap.set_description("Generate metadata files for my C++ GameEngine.");
        ap.refer(&mut options.verbose)
            .add_option(&["-v", "--verbose"], StoreTrue, "Be verbose");
        ap.refer(&mut options.no_output)
            .add_option(&["--no-output"], StoreTrue, "Don't output type_db.");
        ap.refer(&mut options.no_report)
            .add_option(&["--no-report"], StoreTrue, "Don't output type report.");
        ap.refer(&mut options.output_file)
            .add_option(&["-o", "--output"], StoreOption, "Output file");
        ap.refer(&mut options.additional_include_directories)
            .add_option(&["-i", "--include"], Collect, "Additional include directories.");
        ap.refer(&mut options.input_directories)
            .add_argument("DIRECTORIES", List, "Input directories")
            .required();

        ap.parse_args_or_exit();
    }

    let options = options;

    let entries = options.input_directories.iter()
            .filter_map(|x| read_dir(x).ok())
            .flat_map(|x| x)
            .filter_map( |x| x.ok() );

    let files: Vec<PathBuf> = entries.filter(|x| x.file_type().unwrap().is_file())
                        .map(|x| x.path() )
                        .filter(|x| x.extension().is_some())
                        .filter(|x| x.extension().unwrap() == "h")
                        .collect();

    match generate_main_file( &files ) {
        Err( err ) => println!( "ERROR: {}", err ),
        Ok(_) => {},
    };

    let arguments = get_clang_arguments( &options.input_directories, &options.additional_include_directories );


    let mut type_info_vec: Vec<TypeInfo> = Vec::new();
    for built_in in get_built_in_types() {
        type_info_vec.push( built_in.clone() );
    }

    let clang = Clang::new().unwrap();
    let index = Index::new(&clang, false, true);

    let tu = index.parser( &"main.h" )
                .arguments( &arguments )
                .parse().unwrap(); // @Cleanup: should not unwrap this...
    
    for entity in tu.get_entity().get_children().iter().filter( |e| !e.is_in_system_header() ) {
        match from_entity( &entity ) {
            Ok( type_info ) => { type_info_vec.push( type_info ); },
            Err( error ) => {
                if options.verbose {
                    println!( "({}): {}", get_source_file( &entity ).unwrap_or_else(|| String::from("NoFile")), error ); 
                }
            },
        };
    }

    if !options.no_report {
        show_report_types(&type_info_vec);
    }

    if !options.no_output {
        match write_header( &type_info_vec ) {
            Ok(_) => {},
            Err( err ) => { println!( "{}", err ); },
        }

        match write_implementation( &type_info_vec ) {
            Ok(_) => {},
            Err( err ) => { println!( "{}", err ); },
        }
    }

    let duration = start.elapsed();
    let nanos = duration.subsec_nanos() as f64;
    let s = (1000.0*1000.0*1000.0 * (duration.as_secs() as f64) + nanos)/(1000.0 * 1000.0 * 1000.0 );
    println!( "Finished in {}s.", s );
}
