extern crate argparse;
extern crate clang;

use std::fs::read_dir;
// use std::time::{Duration, Instant};
use argparse::{ArgumentParser, StoreTrue, StoreOption, List};
use clang::*;

struct Options {
    verbose: bool,
    input_directories: Vec<String>,
    output_file: Option<String>,
}

struct TypeField {
    name: String,
    type_name: String,
}

struct TypeInfo {
    name: String,
    parent: Option<String>,
    fields: Vec<TypeField>,
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

    let mut parsed_types: Vec<TypeInfo> = vec!();
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
        // Get the structs in this translation unit
        let structs = tu.get_entity().get_children().into_iter().filter(|e| {
            e.get_kind() == EntityKind::StructDecl
        }).collect::<Vec<_>>();

        // Print information about the structs
        for struct_ in structs {
            if !struct_.is_in_main_file() {continue};
            if !struct_.is_definition()  {continue};

            if let Some( name ) = struct_.get_name() {
                let mut type_info = TypeInfo { name: name.clone(), fields: vec!(), parent: None };

                if let Some( parent ) = struct_.get_children()
                                                .into_iter()
                                                .filter( |x| x.get_kind() == EntityKind::BaseSpecifier )
                                                .nth(0) {
                    type_info.parent = parent.get_name();
                }

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

                parsed_types.push( type_info );
            }
        }
    }

    for parsed_type in parsed_types {
        print!("Type: {}", parsed_type.name);
        if let Some( parent ) = parsed_type.parent {
            println!(" (parent: {})", parent);
        } else {
            println!();
        }
        for field in parsed_type.fields {
            println!("    Field: {} ({})", field.name, field.type_name);
        }
    }
}
