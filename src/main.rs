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

struct Options {
    verbose: bool,
    input_directories: Vec<String>,
    output_file: Option<String>,
}

#[derive(Clone)]
struct TypeInfo {
    name: String,
    _struct: Option<TypeInfoStruct>,
    _field: Option<TypeInfoField>,
    _scalar: Option<TypeInfoScalar>,
}

#[derive(Clone)]
struct TypeInfoStruct {
    fields: Vec<TypeInfo>,
    parent: Option<String>,
}

#[derive(Clone)]
struct TypeInfoField {
    field_name: String
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

impl TypeInfo {
    fn from( name: &str ) -> TypeInfo {
        TypeInfo{ name: String::from(name), _struct: None, _field: None, _scalar: None }
    }

    fn new( name: &String ) -> TypeInfo {
        TypeInfo{ name: name.clone(), _struct: None, _field: None, _scalar: None }
    }

    fn make_scalar( mut self, scalar_type: ScalarType ) -> TypeInfo {
        self._scalar = Some( TypeInfoScalar { scalar_type: scalar_type } );
        self
    }

    fn make_field( mut self, field_name: &str ) -> TypeInfo {
        self._field = Some( TypeInfoField{ field_name: String::from( field_name ) } );
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
}

fn from_entity_structdecl( entity: &Entity ) -> Result<TypeInfo, &'static str> {
    match ( entity.get_name(), entity.get_type() ) {
        ( Some( name ), Some( type_def ) ) => {
            let mut type_info = TypeInfo::new( &name ).make_struct( &None );

            {
                let mut type_info_struct = type_info._struct.as_mut().unwrap();
                if let Some( parent_ent ) = entity.get_children().into_iter().filter(|x| x.get_kind() == EntityKind::BaseSpecifier).nth(0) {
                    if let Some( type_def ) = parent_ent.get_type() {
                        type_info_struct.parent = Some( type_def.get_display_name().clone() );
                    }
                }

                if let Some( fields ) = type_def.get_fields() {
                    for field in fields {
                        match from_entity( &field ) {
                            Ok( type_field ) => { type_info_struct.fields.push( type_field ); },
                            Err( err ) => return Err( err ),
                        }
                    }
                }
            }

            let type_info = type_info;
            Ok( type_info )
        },
        ( None, Some(_) ) => Err( "Couldn't generate a TypeInfo from this StructDecl (missing name)."),
        ( Some(_), None ) => Err( "Couldn't generate a TypeInfo from this StructDecl (missing type)."),
        ( None, None ) => Err( "Couldn't generate a TypeInfo from this StructDecl." ),
    }
}

fn from_entity_fielddecl( entity: &Entity ) -> Result<TypeInfo, &'static str> {
    match ( entity.get_name(), entity.get_type() ) {
        ( Some( name ), Some( type_def ) ) => Ok( TypeInfo::from( &type_def.get_display_name() ).make_field( &name ) ),
        _ => Err( "Couldn't generate Field from this FieldDecl." ),
    }
}

fn from_entity( entity: &Entity ) -> Result<TypeInfo, &'static str> {
    match entity.get_kind() {
        EntityKind::StructDecl => from_entity_structdecl( entity ),
        EntityKind::FieldDecl => from_entity_fielddecl( entity ),
        _ => Err( "Couldn't recognize entity type" ),
    }
}



fn get_built_in_types() -> Vec<TypeInfo> {
    use ScalarType::*;
    let built_ins: Vec<TypeInfo> = vec![
        TypeInfo::from("int").make_scalar(INT),
        TypeInfo::from("float").make_scalar(FLOAT),
        TypeInfo::from("double").make_scalar(FLOAT),
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
        output_file: None
    };

    {
        let mut ap = ArgumentParser::new();
        ap.set_description("Generate metadata files for my C++ GameEngine.");
        ap.refer(&mut options.verbose)
            .add_option(&["-v", "--verbose"], StoreTrue, "Be verbose");
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
        
        let structs = tu.get_entity().get_children().into_iter().filter(|e| { e.get_kind() == EntityKind::StructDecl }).collect::<Vec<_>>();

        for struct_ in structs {
            if !(struct_.is_in_main_file() && struct_.is_definition()) {continue};

            match from_entity( &struct_ ) {
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

    for type_info in type_infos_map.values().filter( |x| x._scalar.is_some() ) {
        print!("Type: {}", type_info.name);
        let type_scalar = type_info._scalar.as_ref().unwrap();
        println!( " ({})", type_scalar.scalar_type );
    }


    let duration = start.elapsed();
    let nanos = duration.subsec_nanos() as f64;
    let s = (1000.0*1000.0*1000.0 * (duration.as_secs() as f64) + nanos)/(1000.0 * 1000.0 * 1000.0 );
    println!( "Finished in {}s.", s );
}
