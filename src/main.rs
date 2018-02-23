extern crate argparse;
extern crate clang;

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
}

fn from_entity( entity: &Entity ) -> Result<TypeInfo, &'static str> {
    match ( entity.get_name(), entity.get_type() ) {
        ( Some( name ), Some( type_def ) ) => {
            let mut type_info = TypeInfo::new( &name );

            if let Some( fields ) = type_def.get_fields() {
                for field in fields {
                    if field.get_name() == None || field.get_type() == None { continue; }

                    let type_field = TypeInfo::new( &field.get_name().unwrap() );
                    // TypeField{ name: field.get_name().unwrap(), type_name: field.get_type().unwrap().get_display_name() };
                    // type_info.fields.push( type_field );
                }
            }

            if let Some( parent ) = entity.get_children().into_iter().filter(|x| x.get_kind() == EntityKind::BaseSpecifier).nth(0) {
                if let Some( type_def ) = parent.get_type() {
                    // type_info.parent = Some( type_def.get_display_name() );
                }
            }

            let type_info = type_info;
            return Ok( type_info );
        },
        ( None, Some(_) ) => return Err( "Couldn't generate a TypeInfo from this entity (missing name)."),
        ( Some(_), None ) => return Err( "Couldn't generate a TypeInfo from this entity (missing type)."),
        ( None, None ) => return Err( "Couldn't generate a TypeInfo from this entity." ),
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

/*
    for (_, type_info) in type_infos_map {
        print!("Type: {}", type_info.name);
        if let Some( parent ) = type_info.parent {
            println!(" (parent: {})", parent);
        } else {
            println!();
        }
        for field in type_info.fields {
            println!("    Field: {} ({})", field.name, field.type_name);
        }
    }
*/

    let duration = start.elapsed();
    let nanos = duration.subsec_nanos() as f64;
    let s = (1000.0*1000.0*1000.0 * (duration.as_secs() as f64) + nanos)/(1000.0 * 1000.0 * 1000.0 );
    println!( "Finished in {}s.", s );
}
