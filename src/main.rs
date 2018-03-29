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
    _struct:     Option<TypeInfoStruct>,
    _field:      Option<TypeInfoField>,
    _scalar:     Option<TypeInfoScalar>,
    _enum:       Option<TypeInfoEnum>,
    _func:       Option<TypeInfoFunc>,
}

#[derive(Clone, Default)]
struct TypeInfoStruct {
    fields: Vec<TypeInfo>,
    functions: Vec<TypeInfo>,
    parent: Option<String>,
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
    FLOAT
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

impl Display for ScalarInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ScalarInfo::*;
        match *self {
            INT => write!(f, "int"),
            UINT => write!(f, "uint"),
            FLOAT => write!(f, "float"),
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

    fn make_struct( mut self, parent: &Option<&str> ) -> TypeInfo {
        self._struct = Some( TypeInfoStruct{ parent: parent.map(|x| x.to_string()), ..Default::default() });
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

fn from_entity_structdecl( entity: &Entity ) -> Result<TypeInfo, GMError> {
    if !entity.is_definition() { return Err( GMError::info( "not definition".to_string() ) ); }
    match ( entity.get_name(), entity.get_type() ) {
        ( Some( name ), Some( _ ) ) => {
            let mut type_info = TypeInfo::new( name ).make_struct( &None );
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
        ( false, Some( type_def ) ) => Err( GMError::error( format!("Enum {} is unscoped, can't generate TypeInfo for unscoped enums.", type_def.get_display_name() ) ) ),
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

fn from_entity( entity: &Entity ) -> Result<TypeInfo, GMError> {
    match entity.get_kind() {
        EntityKind::StructDecl   => from_entity_structdecl( entity ),
        EntityKind::ClassDecl    => from_entity_structdecl( entity ),
        EntityKind::FieldDecl    => from_entity_fielddecl( entity, Some( &entity.get_semantic_parent().unwrap().get_type().unwrap() ) ), // should be ok for a field decl... haven't found a better way to pass this...
        EntityKind::EnumDecl     => from_entity_enumdecl( entity ),
        EntityKind::FunctionDecl => from_entity_funcdecl( entity ),
        EntityKind::ParmDecl     => from_entity_fielddecl( entity, None ),
        EntityKind::Method       => from_entity_funcdecl( entity ),
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

    for type_info in type_info_vec.iter().filter( |t| t._enum.is_some() ) {
        let enum_type = type_info._enum.as_ref().unwrap();
        writeln!( file, "enum class {} : {};", type_info.name, enum_type.underlying_type )?;
    }

    writeln!( file )?;
    
    for type_info in type_info_vec.iter().filter( |t| t._struct.is_some() ) {
        writeln!( file, "struct {};", type_info.name )?;
    }

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

    write!( file, "\n" )?;

    for type_info in type_info_vec.iter() {
        if type_info._func.is_some() { continue; }
        writeln!( file, "template<> const TypeInfo* type_of<{}>();", type_info.name )?;
        writeln!( file, "const TypeInfo* type_of(const {}& obj);", type_info.name )?;
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
        false => result.join( "|" )
    }
}

fn write_field_implementation( type_info_map: &HashMap<String, &TypeInfo>, file: &mut File, field: &TypeInfo, indent: usize ) -> Result<bool, GMError> {

    if let Some( type_field ) = field._field.as_ref() {
        let field_name = &type_field.field_name;

        if let Some( ref template_args ) = type_field.templates {
            writeln!( file, "{indent}FieldInfo( \"{field_name}\", \"{template_name}\", {{", indent = " ".repeat( indent * 4 ), field_name = field_name, template_name = field.name )?;
            for arg in template_args {
                write_field_implementation( type_info_map, file, &arg, indent+1 )?;
            }
            writeln!( file, "{indent}}}, (FieldInfo_Modifier) ({modifier}), {offset} ),", indent = " ".repeat( indent * 4 ), modifier = build_modifier_string( &type_field ), offset = type_field.offset )?;
        }
        else {
            if type_info_map.get( &field.name ).is_some() {
                writeln!( file, "{indent}FieldInfo( \"{field_name}\", type_of<{field_type}>(), (FieldInfo_Modifier) ({modifier}), {offset} ),", indent = " ".repeat( indent * 4 ), field_name = field_name, field_type = field.name, modifier = build_modifier_string( &type_field ), offset = type_field.offset )?;
            }
            else {
                writeln!( file, "{indent}FieldInfo( \"{field_name}\", nullptr, (FieldInfo_Modifier) ({modifier}), {offset} ),", indent = " ".repeat( indent * 4 ), field_name = field_name, modifier = build_modifier_string( &type_field ), offset = type_field.offset )?;
            }
        }
    }
    else {
        if type_info_map.get( &field.name ).is_some() {
            writeln!( file, "{indent}FieldInfo( \"\", type_of<{field_type}>(), FieldInfo_Modifier::None, 0 ),", indent = " ".repeat( indent * 4 ), field_type = field.name )?;
        }
        else {
            writeln!( file, "{indent}FieldInfo( \"\", nullptr, FieldInfo_Modifier::None, 0 ),", indent = " ".repeat( indent * 4 ) )?;
        }
    }

    Ok( true )
}

fn write_struct_implementation( type_info_map: &HashMap<String, &TypeInfo>, file: &mut File, type_info: &TypeInfo ) -> Result<bool, GMError> {

    let struct_type = type_info._struct.as_ref().unwrap();
    match struct_type.parent {
        Some( ref parent ) => writeln!( file, "static StructInfo type_{struct_name}( \"{struct_name}\", sizeof({struct_name}), static_cast<const StructInfo*>( type_of<{parent_name}>() ), {{", struct_name = type_info.name, parent_name = parent )?,
        None => writeln!( file, "static StructInfo type_{struct_name}( \"{struct_name}\", sizeof({struct_name}), nullptr, std::vector<FieldInfo> {{", struct_name = type_info.name )?,
    };

    // fields
    for field in &struct_type.fields {
        write_field_implementation( type_info_map, file, &field, 1 )?;
    }

    writeln!( file, "}}, {{")?;
    // functions
    for function in &struct_type.functions {
        let type_func = function._func.as_ref().unwrap();
        let func_name = &function.name;
        if let Some( ref return_type ) = type_func.return_type {
            let return_type = &return_type;
            writeln!( file, "    FuncInfo( \"{func_name}\", type_of<{return_type}>(), {{", func_name = func_name, return_type = return_type.name)? // incomplete informations sent to C++... return_type needs to be a FieldInfo
        }
        else {
            writeln!( file, "    FuncInfo( \"{func_name}\", nullptr, {{", func_name = func_name )?
        }

        for param in &type_func.parameters {
            write_field_implementation( type_info_map, file, param, 2 )?;
        }
        writeln!( file, "    }} ),")?;
    }
    writeln!( file,  "}} );" )?;
    writeln!( file, "template<> const TypeInfo* type_of<{struct_name}>() {{ return static_cast<TypeInfo*>( &type_{struct_name} ); }}", struct_name = type_info.name )?;

    // Write constructors if needed
    if has_object_parent( struct_type, type_info_map ) {
        let parent = struct_type.parent.as_ref().unwrap();
        writeln!( file, "{struct_name}::{struct_name}() : {parent_name}( type_{struct_name}.object_data.object_id ) {{}}", struct_name = type_info.name, parent_name = parent )?;
        writeln!( file, "{struct_name}::{struct_name}( u32 _type_id ) : {parent_name}( _type_id ) {{}}", struct_name = type_info.name, parent_name = parent )?;
    }

    writeln!( file )?;

    Ok( true )
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

    // scalars
    for type_info in type_info_vec.iter().filter( |t| t._scalar.is_some() ) {
        use ScalarInfo::*;

        let scalar_type = type_info._scalar.as_ref().unwrap();
        let scalar_name = &type_info.name;

        let scalar_type_name = match scalar_type.scalar_type {
            INT => "INT",
            UINT => "UINT",
            FLOAT => "FLOAT",
        };
        writeln!( file, "static ScalarInfo type_{scalar_name} ( sizeof( {scalar_name} ), ScalarInfo_Type::{scalar_type} );", scalar_name = scalar_name, scalar_type = scalar_type_name )?;
        writeln!( file, "template<> const TypeInfo* type_of<{scalar_name}>() {{ return &type_{scalar_name}; }}", scalar_name = scalar_name )?;
        writeln!( file, "const TypeInfo* type_of( const {scalar_name}& obj ) {{ return &type_{scalar_name}; }}", scalar_name = scalar_name )?;
        writeln!( file )?;
    }

    // enums
    for type_info in type_info_vec.iter().filter( |t| t._enum.is_some() ) {
        let enum_type = type_info._enum.as_ref().unwrap();
        writeln!( file, "static EnumInfo type_{enum_name} ( \"{enum_name}\", type_of<{underlying_type}>(), {{", enum_name=type_info.name, underlying_type=enum_type.underlying_type )?;
        for ( name, &(value, _) ) in &enum_type.enum_values {
            writeln!( file, "    {{ \"{}\", {} }},", name, value )?;
        }
        writeln!( file, "}} );" )?;

        writeln!(file, "template<> const TypeInfo* type_of<{enum_name}>() {{ return &type_{enum_name}; }}", enum_name=type_info.name )?;
        writeln!(file, "const TypeInfo* type_of( const {enum_name}& obj ) {{ return &type_{enum_name}; }}", enum_name=type_info.name )?;
        writeln!( file )?;
    }

    // structs
    use std::iter::FromIterator;
    let type_info_map: HashMap<String, &TypeInfo> = HashMap::from_iter(type_info_vec.iter().map(|x| (x.name.clone(), x)));

    for type_info in type_info_vec.iter().filter( |t| t._struct.is_some() ) {
        write_struct_implementation( &type_info_map, &mut file, &type_info )?;
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
        TypeInfo::new("char").make_scalar(UINT),
        TypeInfo::new("i8").make_scalar(INT),
        TypeInfo::new("i16").make_scalar(INT),
        TypeInfo::new("i32").make_scalar(INT),
        TypeInfo::new("i64").make_scalar(INT),
        TypeInfo::new("u8").make_scalar(UINT),
        TypeInfo::new("u16").make_scalar(UINT),
        TypeInfo::new("u32").make_scalar(UINT),
        TypeInfo::new("u64").make_scalar(UINT),
        TypeInfo::new("f32").make_scalar(FLOAT),
        TypeInfo::new("f64").make_scalar(FLOAT),
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
                .parse().unwrap();
    
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
        for type_info in type_info_vec.iter().filter( |x| x._struct.is_some() ) {
            print!("Type: {}", type_info.name);
            let type_struct = type_info._struct.as_ref().unwrap();

            if let Some( ref parent ) = type_struct.parent {
                println!(" (parent: {})", parent);
            } else {
                println!();
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

        for type_info in type_info_vec.iter().filter( |x| x._enum.is_some() ) {
            let type_enum = type_info._enum.as_ref().unwrap();
            println!("Enum: {} ({})", type_info.name, type_enum.underlying_type);

            for (name, &(_, uval)) in &type_enum.enum_values {
                println!("    {}: {}", name, uval);
            }
        }

        for type_info in type_info_vec.iter().filter( |x| x._scalar.is_some() ) {
            print!("Type: {}", type_info.name);
            let type_scalar = type_info._scalar.as_ref().unwrap();
            println!( " ({})", type_scalar.scalar_type );
        }

        for _type_info in type_info_vec.iter().filter( |x| x._func.is_some() ) {
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
