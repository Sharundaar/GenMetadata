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
struct TypeField {
    name: String,
    type_name: String,
}

#[derive(Clone)]
struct TypeInfo {
    name: String,
    parent: Option<String>,
    fields: Vec<TypeField>,
    is_complete: bool,
}

impl TypeInfo {
    fn new( name: &str ) -> TypeInfo {
        return TypeInfo{ name: name.to_string(), parent: None, fields: vec!(), is_complete: true };
    }
}

fn from_entity( entity: &Entity ) -> Result<TypeInfo, &'static str> {
    if let Some( name ) = entity.get_name() {
        if let Some( type_def ) = entity.get_type() {
            let mut type_info = TypeInfo {
                name: name,
                parent: None,
                fields: vec!(),
                is_complete: true,
            };

            if let Some( fields ) = type_def.get_fields() {
                for field in fields {
                    if field.get_name() == None || field.get_type() == None { continue; }

                    let type_field = TypeField{ name: field.get_name().unwrap(), type_name: field.get_type().unwrap().get_display_name() };
                    type_info.fields.push( type_field );
                }
            }

            if let Some( parent ) = entity.get_children().into_iter().filter(|x| x.get_kind() == EntityKind::BaseSpecifier).nth(0) {
                if let Some( type_def ) = parent.get_type() {
                    type_info.parent = Some( type_def.get_display_name() );
                }
            }

            let type_info = type_info;
            return Ok( type_info );
        }
    }

    return Err( "Couldn't generate a TypeInfo from this entity." );
}



fn get_built_in_types() -> Vec<TypeInfo> {
    let built_ins: Vec<TypeInfo> = vec![
        TypeInfo::new("int"),
        TypeInfo::new("float"),
        TypeInfo::new("double"),
        TypeInfo::new("i8"),
        TypeInfo::new("i16"),
        TypeInfo::new("i32"),
        TypeInfo::new("i64"),
        TypeInfo::new("u8"),
        TypeInfo::new("u16"),
        TypeInfo::new("u32"),
        TypeInfo::new("u64"),
        TypeInfo::new("f32"),
        TypeInfo::new("f64"),
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

    let duration = start.elapsed();
    let nanos = duration.subsec_nanos() as f64;
    let s = (1000.0*1000.0*1000.0 * (duration.as_secs() as f64) + nanos)/(1000.0 * 1000.0 * 1000.0 );
    println!( "Finished in {}s.", s );
}
