extern crate argparse;
extern crate clang;

use std::fs::read_dir;
// use std::time::{Duration, Instant};
use argparse::{ArgumentParser, StoreTrue, StoreOption, List};
use clang::*;
use std::collections::HashMap;

struct Options {
    verbose: bool,
    input_directories: Vec<String>,
    output_file: Option<String>,
}

struct TypeField<'a> {
    name: String,
    type_name: &<'a> TypeInfo,
}

struct TypeInfo<'a> {
    name: String,
    parent: Option<&<'a>TypeInfo>,
    fields: Vec<TypeField>,
    is_complete: bool,
}

fn make_type_info( name: &str ) -> TypeInfo {
    return TypeInfo{ name: name.to_string(), parent: None, fields: vec!(), is_complete: true };
}

fn from_entity( &entity: Entity, mut& type_info_map: HashMap<String, TypeInfo> ) -> Result<&TypeInfo> {
    if let Some(name) = entity.get_name() {
        if let Some(type) = entity.get_type() {
            let mut type_info = TypeInfo {
                name: type.get_display_name().clone(),
                parent = None,
                fields: vec!(),
                is_complete: false,
            }

            let type_info = type_info;

            type_info_map.insert( type_info.name.clone(), type_info );
            return Ok(&type_info_map.get( name ));
        }
    }

    return Err( "Couldn't generate a TypeInfo from this entity." );
}

fn get_built_in_types() -> Vec<TypeInfo> {
    let built_ins: Vec<TypeInfo> = vec!(
        make_type_info( "int" ),
        make_type_info( "float" ),
        make_type_info( "double" ),
        make_type_info( "i8" ),
        make_type_info( "i16" ),
        make_type_info( "i32" ),
        make_type_info( "i64" ),
        make_type_info( "u8" ),
        make_type_info( "u16" ),
        make_type_info( "u32" ),
        make_type_info( "u64" ),
        make_type_info( "f32" ),
        make_type_info( "f64" ),
    );
    return built_ins;
}

fn main() {
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

    let mut type_infos_map: HashMap<String, TypeInfo> = HashMap::new();
    for built_in in get_built_in_types() {
        type_infos_map.insert( built_in.name, built_in );
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

            from_entity( &struct_, mut& type_infos_map );
/*
            if let Some( name ) = struct_.get_name() {
                let mut type_info = from_entity;

                println!("struct: {}", &name);
                if let Some( struct_type ) = struct_.get_type() {
                    for field in struct_type.get_fields().unwrap() {
                        if let Some( t ) = field.get_type() {
                            println!("type kind: {:?}", t.get_kind());
                        if let Some( name ) = field.get_name() {
                            let type_field = TypeField {
                                name: name,
                                type_name: t.get_display_name(),
                            };
                            type_info.fields.push( type_field );
                        }
                        }
                    }
                }
            }
*/
        }
    }

    for (_, typeInfo) in type_infos_map {
        print!("Type: {}", typeInfo.name);
        if let Some( parent ) = typeInfo.parent {
            println!(" (parent: {})", parent.name);
        } else {
            println!();
        }
        for field in typeInfo.fields {
            println!("    Field: {} ({})", field.name, field.type_name);
        }
    }
}
