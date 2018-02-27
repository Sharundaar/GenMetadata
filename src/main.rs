extern crate argparse;
extern crate clang;

use std::fmt;
use std::fmt::Display;
use std::fs::read_dir;
use std::time::Instant;
use argparse::{ArgumentParser, StoreTrue, StoreOption, List};
use clang::*;
use std::collections::BTreeMap;
use std::result::Result;
use std::fs::File;
use std::io::Write;
use std::collections::HashSet;
use std::collections::HashMap;

struct Options {
    verbose: bool,
    input_directories: Vec<String>,
    output_file: Option<String>,
    no_output: bool,
}

#[derive(Clone)]
struct TypeInfo {
    name: String,
    source_file: Option<String>,
    _struct:     Option<TypeInfoStruct>,
    _field:      Option<TypeInfoField>,
    _scalar:     Option<TypeInfoScalar>,
    _enum:       Option<TypeInfoEnum>,
}

#[derive(Clone)]
struct TypeInfoStruct {
    fields: Vec<TypeInfo>,
    parent: Option<String>,
}

#[derive(Clone)]
struct TypeInfoField {
    field_name: String,
    is_const: bool,
    is_private: bool,
    is_ptr: bool,
    is_ref: bool,
}

#[derive(Clone)]
enum ScalarType {
    INT,
    UINT,
    FLOAT
}

impl Display for ScalarType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ScalarType::*;
        match *self {
            INT => write!(f, "int"),
            UINT => write!(f, "uint"),
            FLOAT => write!(f, "float"),
        }
    }
}

#[derive(Clone)]
struct TypeInfoScalar {
    scalar_type: ScalarType
}

#[derive(Clone)]
struct TypeInfoEnum {
    underlying_type: String,
    enum_values: HashMap<String, (i64, u64)>,
}

impl TypeInfo {
    fn from( name: &str ) -> TypeInfo {
        TypeInfo{ name: String::from(name), source_file: None, _struct: None, _field: None, _scalar: None, _enum: None }
    }

    fn new( name: &String ) -> TypeInfo {
        TypeInfo{ name: name.clone(), source_file: None, _struct: None, _field: None, _scalar: None, _enum: None }
    }

    fn make_scalar( mut self, scalar_type: ScalarType ) -> TypeInfo {
        self._scalar = Some( TypeInfoScalar { scalar_type: scalar_type } );
        self
    }

    fn make_field( mut self, field_name: &str ) -> TypeInfo {
        self._field = Some( TypeInfoField{ field_name: String::from( field_name ), is_const: false, is_private: false, is_ptr: false, is_ref: false } );
        self
    }

    fn make_struct( mut self, parent: &Option<&str> ) -> TypeInfo {
        let mut type_struct = TypeInfoStruct{ parent: None, fields: vec!() };
        if let &Some( parent ) = parent {
            type_struct.parent = Some( String::from( parent ) );
        }
        self._struct = Some(type_struct);
        self
    }

    fn make_enum( mut self, underlying_type: &str ) -> TypeInfo {
        let mut type_enum = TypeInfoEnum{ underlying_type: String::from( underlying_type ), enum_values: HashMap::new() };
        self._enum = Some(type_enum);
        self
    }
}

fn has_object_parent( struct_info: &TypeInfoStruct, type_info_map: &BTreeMap<String, TypeInfo> ) -> bool {
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

fn from_entity_structdecl( entity: &Entity ) -> Result<TypeInfo, String> {
    match ( entity.get_name(), entity.get_type() ) {
        ( Some( name ), Some( type_def ) ) => {
            let mut type_info = TypeInfo::new( &name ).make_struct( &None );
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
                                Err( err ) => {},
                            }
                        },
                        _ => {}
                    }
                }
            }

            let type_info = type_info;
            Ok( type_info )
        },
        ( None, Some(_) ) => Err( "Couldn't generate a TypeInfo from this StructDecl (missing name).".to_string() ),
        ( Some(_), None ) => Err( "Couldn't generate a TypeInfo from this StructDecl (missing type).".to_string() ),
        ( None, None ) => Err( "Couldn't generate a TypeInfo from this StructDecl.".to_string() ),
    }
}

fn from_entity_fielddecl( entity: &Entity ) -> Result<TypeInfo, String> {
    match ( entity.get_name(), entity.get_type() ) {
        ( Some( name ), Some( type_def ) ) => {
            let mut type_info = TypeInfo::from( &type_def.get_display_name().replace( "const", "" ).replace("*", "").trim() ).make_field( &name );
            {
                let mut field_info = type_info._field.as_mut().unwrap();

                use Accessibility::*;
                match entity.get_accessibility().unwrap() {
                    Private | Protected => field_info.is_private = true,
                    Public              => field_info.is_private = false,
                }

                field_info.is_const = type_def.is_const_qualified();

                use TypeKind::*;
                match type_def.get_kind() {
                    Pointer => {
                        field_info.is_ptr = true;
                    },
                    _ => {},
                }
            }
            let type_info = type_info;
            Ok( type_info )
        },
        _ => Err( "Couldn't generate Field from this FieldDecl.".to_string() ),
    }
}

fn from_entity_enumdecl( entity: &Entity ) -> Result<TypeInfo, String> {
    match ( entity.is_scoped(), entity.get_type() ) {
        ( true, Some( type_def ) ) => {
            let mut type_info = TypeInfo::from( &type_def.get_display_name() ).make_enum( &entity.get_enum_underlying_type().unwrap().get_display_name() );
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
        ( false, Some( type_def ) ) => Err( format!("Enum {} is unscoped, can't generate TypeInfo for unscoped enums.", type_def.get_display_name() ) ),
        _ => Err( "Couldn't generate TypeInfo from this EnumDecl.".to_string() ),
    }
}

fn from_entity( entity: &Entity ) -> Result<TypeInfo, String> {
    match entity.get_kind() {
        EntityKind::StructDecl => from_entity_structdecl( entity ),
        EntityKind::FieldDecl  => from_entity_fielddecl( entity ),
        EntityKind::EnumDecl   => from_entity_enumdecl( entity ),
        kind => Err( format!( "Unhandled entity kind: {:?}", kind) ),
    }
}

fn write_header( type_info_map : &BTreeMap<String, TypeInfo> ) -> Result<bool, String>
{
    let mut file = match File::create( "type_db.h" ) {
        Ok( file ) => file,
        Err( _ ) => return Err( "Something bad happend".to_string() ),
    };

    writeln!( file, "#pragma once\n" );

    for type_info in type_info_map.values().filter( |t| t._enum.is_some() ) {
        let enum_type = type_info._enum.as_ref().unwrap();
        writeln!( file, "enum class {} : {};", type_info.name, enum_type.underlying_type );
    }

    writeln!( file );

    for type_info in type_info_map.values().filter( |t| t._struct.is_some() ) {
        writeln!( file, "struct {};", type_info.name );
    }

    write!( file, "\n" );

    for type_info in type_info_map.values() {
        writeln!( file, "template<> const TypeInfo* type_of<{}>();", type_info.name );
        writeln!( file, "const TypeInfo* type_of(const {}& obj);", type_info.name );
        writeln!( file );
    }

    Ok( true )
}

fn build_modifier_string( field: &TypeInfoField ) -> String {
    let mut result: Vec<&str> = vec!();

    if field.is_const   { result.push( "MemberType_Modifier::CONST" ); }
    if field.is_ptr     { result.push( "MemberType_Modifier::POINTER" ); }
    if field.is_private { result.push( "MemberType_Modifier::PRIVATE" ); }
    if field.is_ref     { result.push( "MemberType_Modifier::REFERENCE" ); }


    match result.is_empty() {
        true  => String::from( "MemberType_Modifier::NONE" ),
        false => result.join( "|" )
    }
}

fn write_struct_implementation( type_info_map: &BTreeMap<String, TypeInfo>, file: &mut File, type_info: &TypeInfo ) -> Result<bool, String> {

    let struct_type = type_info._struct.as_ref().unwrap();
    match struct_type.parent {
        Some( ref parent ) => writeln!( file, "static StructType type_{struct_name}( \"{struct_name}\", sizeof({struct_name}), static_cast<const StructType*>( type_of<{parent_name}>() ), {{", struct_name = type_info.name, parent_name = parent ),
        None => writeln!( file, "static StructType type_{struct_name}( \"{struct_name}\", sizeof({struct_name}), nullptr, std::vector<MemberType> {{", struct_name = type_info.name ),
    };

    for field in &struct_type.fields {
        let type_field = field._field.as_ref().unwrap();
        let field_name = &type_field.field_name;
        if type_info_map.get( &field.name ).is_some() {
            writeln!( file, "\tMemberType( \"{field_name}\", type_of<{field_type}>(), {modifier} ),", field_name = field_name, field_type = field.name, modifier = build_modifier_string( &type_field ) );
        }
    }

    writeln!( file, "}} );" );
    writeln!( file, "template<> const TypeInfo* type_of<{struct_name}>() {{ return static_cast<TypeInfo*>( &type_{struct_name} ); }}", struct_name = type_info.name );

    // Write constructors if needed
    if has_object_parent( struct_type, type_info_map ) {
        let parent = struct_type.parent.as_ref().unwrap();
        writeln!( file, "{struct_name}::{struct_name}() : {parent_name}( type_{struct_name}.struct_id ) {{}}", struct_name = type_info.name, parent_name = parent );
        writeln!( file, "{struct_name}::{struct_name}( u32 _type_id ) : {parent_name}( _type_id ) {{}}", struct_name = type_info.name, parent_name = parent );
    }

    writeln!( file );

    Ok( true )
}

fn write_implementation( type_info_map: &BTreeMap<String, TypeInfo> ) -> Result<bool, String> {
    let mut file = match File::create( "type_db.cpp" ) {
        Ok( file ) => file,
        Err( _ ) => return Err( "Something bad happend".to_string() ),
    };

    // includes
    {
        let mut includes: HashSet<&str> = [ "type_db.h", "basic_types.h" ].iter().cloned().collect();

        for type_info in type_info_map.values() {
            if let Some( ref source_file ) = type_info.source_file {
                includes.insert( source_file );
            }
        }

        for include in includes.iter() {
            writeln!( file, "#include \"{}\"", include );
        }
        writeln!( file, "" );
    }

    // scalars
    for type_info in type_info_map.values().filter( |t| t._scalar.is_some() ) {
        use ScalarType::*;

        let scalar_type = type_info._scalar.as_ref().unwrap();
        let scalar_name = &type_info.name;

        let scalar_type_name = match scalar_type.scalar_type {
            INT => "INT",
            UINT => "UINT",
            FLOAT => "FLOAT",
        };
        writeln!( file, "static ScalarType type_{scalar_name} ( sizeof( {scalar_name} ), ScalarType_Type::{scalar_type} );", scalar_name = scalar_name, scalar_type = scalar_type_name );
        writeln!( file, "template<> const TypeInfo* type_of<{scalar_name}>() {{ return &type_{scalar_name}; }}", scalar_name = scalar_name );
        writeln!( file, "const TypeInfo* type_of( const {scalar_name}& obj ) {{ return &type_{scalar_name}; }}", scalar_name = scalar_name );
        writeln!( file );
    }

    // enums
    for type_info in type_info_map.values().filter( |t| t._enum.is_some() ) {
        let enum_type = type_info._enum.as_ref().unwrap();
        writeln!( file, "static EnumType type_{enum_name} ( \"{enum_name}\", type_of<{underlying_type}>(), {{", enum_name=type_info.name, underlying_type=enum_type.underlying_type );
        for ( name, &(value, _) ) in &enum_type.enum_values {
            writeln!( file, "    {{ \"{}\", {} }},", name, value );
        }
        writeln!( file, "}} );" );

        writeln!(file, "template<> const TypeInfo* type_of<{enum_name}>() {{ return &type_{enum_name}; }}", enum_name=type_info.name );
        writeln!(file, "const TypeInfo* type_of( const {enum_name}& obj ) {{ return &type_{enum_name}; }}", enum_name=type_info.name );
        writeln!( file );
    }

    // structs
    for type_info in type_info_map.values().filter( |t| t._struct.is_some() ) {
        write_struct_implementation( &type_info_map, &mut file, &type_info );
    }


    Ok( true )
}


fn get_built_in_types() -> Vec<TypeInfo> {
    use ScalarType::*;
    let built_ins: Vec<TypeInfo> = vec![
        TypeInfo::from("i8").make_scalar(INT),
        TypeInfo::from("i16").make_scalar(INT),
        TypeInfo::from("i32").make_scalar(INT),
        TypeInfo::from("i64").make_scalar(INT),
        TypeInfo::from("u8").make_scalar(UINT),
        TypeInfo::from("u16").make_scalar(UINT),
        TypeInfo::from("u32").make_scalar(UINT),
        TypeInfo::from("u64").make_scalar(UINT),
        TypeInfo::from("f32").make_scalar(FLOAT),
        TypeInfo::from("f64").make_scalar(FLOAT),
    ];

    return built_ins;
}

fn main() {
    let start = Instant::now();

    let mut options : Options = Options{
        verbose: false,
        input_directories: vec!(),
        output_file: None,
        no_output: false,
    };

    {
        let mut ap = ArgumentParser::new();
        ap.set_description("Generate metadata files for my C++ GameEngine.");
        ap.refer(&mut options.verbose)
            .add_option(&["-v", "--verbose"], StoreTrue, "Be verbose");
        ap.refer(&mut options.no_output)
            .add_option(&["--no-output"], StoreTrue, "Don't output type_db.");
        ap.refer(&mut options.output_file)
            .add_option(&["-o", "--output"], StoreOption, "Output file");
        ap.refer(&mut options.input_directories)
            .add_argument("DIRECTORIES", List, "Input directories")
            .required();

        ap.parse_args_or_exit();
    }

    let options = options;

    let entries = options.input_directories.into_iter()
            .filter_map(|x| read_dir(x).ok())
            .flat_map(|x| x)
            .filter_map( |x| x.ok() );

    let files: Vec<String> = entries.filter(|x| x.file_type().unwrap().is_file())
                        .filter_map(|x| x.path().into_os_string().into_string().ok())
                        .filter(|x| x.ends_with( ".h" ))
                        .collect();

    let mut type_infos_map: BTreeMap<String, TypeInfo> = BTreeMap::new();
    for built_in in get_built_in_types() {
        type_infos_map.insert( built_in.name.clone(), built_in.clone() );
    }

    let clang = Clang::new().unwrap();
    let index = Index::new(&clang, false, true);

    for file in files {
        let tu = index.parser( &file )
                    .arguments( &["-x", "c++", 
                                  "-I..\\GameEngine2\\mathlib\\",
                                  "-I..\\GameEngine2\\includes\\",
                                  "-I..\\Externals\\STB\\",
                                  "-I..\\Externals\\SDL2-2.0.5\\include\\",
                                  "-I..\\Externals\\GLAD\\include\\"] )
                    .parse().unwrap();
        
        for entity in tu.get_entity().get_children().iter().filter( |e| e.is_in_main_file() && e.is_definition() ) {
            match from_entity( &entity ) {
                Ok( type_info ) => { type_infos_map.insert( type_info.name.clone(), type_info ); },
                Err( error ) => { println!( "ERROR ({}): {}", file, error ); },
            };
        }
    }

    for type_info in type_infos_map.values().filter( |x| x._struct.is_some() ) {
        print!("Type: {}", type_info.name);
        let type_struct = type_info._struct.as_ref().unwrap();

        if let Some( ref parent ) = type_struct.parent {
            println!(" (parent: {})", parent);
        } else {
            println!();
        }
        for field in &type_struct.fields {
            println!("    {} ({})", field._field.as_ref().unwrap().field_name, field.name);
        }
    }

    for type_info in type_infos_map.values().filter( |x| x._enum.is_some() ) {
        let type_enum = type_info._enum.as_ref().unwrap();
        println!("Enum: {} ({})", type_info.name, type_enum.underlying_type);

        for (name, &(ival, uval)) in &type_enum.enum_values {
            println!("    {}: {}", name, uval);
        }
    }

    for type_info in type_infos_map.values().filter( |x| x._scalar.is_some() ) {
        print!("Type: {}", type_info.name);
        let type_scalar = type_info._scalar.as_ref().unwrap();
        println!( " ({})", type_scalar.scalar_type );
    }

    if !options.no_output {
        match write_header( &type_infos_map ) {
            Ok(_) => {},
            Err( err ) => { println!( "{}", err ); },
        }

        match write_implementation( &type_infos_map ) {
            Ok(_) => {},
            Err( err ) => { println!( "{}", err ); },
        }
    }

    let duration = start.elapsed();
    let nanos = duration.subsec_nanos() as f64;
    let s = (1000.0*1000.0*1000.0 * (duration.as_secs() as f64) + nanos)/(1000.0 * 1000.0 * 1000.0 );
    println!( "Finished in {}s.", s );
}
