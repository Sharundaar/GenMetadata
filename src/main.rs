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
    output_directory: Option<String>,
    no_output: bool,
    no_report: bool,
}

#[derive(Clone, Default, PartialEq, Eq )]
struct TypeInfoSource {
    file_path: String,
    is_system_file: bool,
}

#[derive(Clone, Default, PartialEq, Eq )]
struct TypeInfo {
    name: String,
    source_file: Option<TypeInfoSource>,
    _struct:     Option<TypeInfoStruct>,
    _field:      Option<TypeInfoField>,
    _scalar:     Option<TypeInfoScalar>,
    _enum:       Option<TypeInfoEnum>,
    _func:       Option<TypeInfoFunc>,
    _template:   Option<TypeInfoTemplate>,
    _typedef:    Option<TypeInfoTypedef>,
}

#[derive(Clone, Default, PartialEq, Eq)]
struct TypeInfoTemplate {
}

#[derive(Clone, Default, PartialEq, Eq)]
struct TypeInfoTypedef {
    source_type: String,
    field: Option<Box<TypeInfo>>,
}

#[derive(Clone, PartialEq, Eq)]
enum TypeInfoStructKind
{
    Struct,
    Class,
}

#[derive(Clone, Default, PartialEq, Eq)]
struct TypeInfoStruct {
    fields: Vec<TypeInfo>,
    functions: Vec<TypeInfo>,
    parent: Option<String>,
    kind: TypeInfoStructKind,
    has_default_constructor: bool,
}

#[derive(Clone, Default, PartialEq, Eq)]
struct TypeInfoField {
    field_name: String,
    templates: Option<Vec<TypeInfo>>,
    offset: u32,
    is_const: bool,
    is_private: bool,
    is_ptr: bool,
    is_ref: bool,
}

#[derive(Clone, PartialEq, Eq)]
enum ScalarInfo {
    INT,
    UINT,
    FLOAT,
    BOOL,
    CHAR
}

#[derive(Clone, Default, PartialEq, Eq)]
struct TypeInfoScalar {
    scalar_type: ScalarInfo
}

#[derive(Clone, Default, PartialEq, Eq)]
struct TypeInfoEnum {
    underlying_type: String,
    enum_values: HashMap<String, (i64, u64)>,
    is_scoped: bool,
}

#[derive(Clone, Default, PartialEq, Eq)]
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


struct GMError {
    gravity: GMErrorGravity,
    description: String,

    #[allow(dead_code)] kind: GMErrorKind,
}

struct ExportContext {
    file: File,
    indentation: String,
    indent_str:  String,

    type_var_override: Vec<String>,
}

#[derive(Default)]
struct TypeInfoStore {
    data: Vec<TypeInfo>,
    map: HashMap<String, usize>
}

impl TypeInfoStore {
    fn reserve_name( &mut self, tin: String ) {
        if !self.map.contains_key( &tin ) {
            self.map.insert( tin, std::usize::MAX );
        }
    }

    fn push( &mut self, ti: TypeInfo ) {
        if self.map.contains_key( &ti.name ) && self.map[&ti.name] != std::usize::MAX {
            let index = self.map[&ti.name];
            self.data[index] = ti;
        } else {
            self.map.insert( ti.name.clone(), self.data.len() );
            self.data.push( ti );
        }
    }

    fn has( &self, name: &String ) -> bool {
        self.map.contains_key( name )
    }

    fn get( &self, name: &String ) -> Option<&TypeInfo> {
        if self.has( name ) {
            self.data.get( self.map[name] )
        } else {
            None
        }
    }
}

#[derive(Default)]
struct ParseContext {
    store: TypeInfoStore
}

impl ParseContext {
    fn store_type_info( &mut self, ti: TypeInfo ) -> &TypeInfo {
        self.store.push( ti );
        self.store.data.last().unwrap()
    }
    fn reserve_type_info_name( &mut self, tin: String ) {
        self.store.reserve_name(tin);
    }
}

macro_rules! gm_writeln {
    ($context:ident) => {$context.newline()};
    ($context:ident, $fmt:expr) => {$context.writeln( format!( $fmt ) )};
    ($context:ident, $fmt:expr, $($arg:tt)*) => {$context.writeln( format!( $fmt, $($arg)* ) )};
}

macro_rules! gm_begin_scope {
    ($context:ident) => {$context.begin_scope()};
}

macro_rules! gm_end_scope {
    ($context:ident) => {$context.end_scope()};
}

macro_rules! gm_error {
    ($fmt:expr) => {Err(GMError::error( String::from($fmt)))};
    ($fmt:expr, $($arg:tt)*) => {Err(GMError::error(format!($fmt,$($arg)*)))};
}

#[allow(unused_macros)]
macro_rules! gm_warning {
    ($fmt:expr) => {Err(GMError::warning( String::from($fmt)))};
    ($fmt:expr, $($arg:tt)*) => {Err(GMError::warning(format!($fmt, $($arg)*)))};
}

macro_rules! gm_info {
    ($fmt:expr) => {Err(GMError::info( String::from( $fmt ) ))};
    ($fmt:expr, $($arg:tt)*) => {Err(GMError::info( format!( $fmt, $($arg)* ) ) ) };
}

impl ExportContext {

    fn new(file: File, indent_str: String) -> ExportContext {
        ExportContext{ indent_str: indent_str, file: file, indentation: String::new(), type_var_override: vec!() }
    }

    fn begin_scope(&mut self) -> Result<(), GMError> {
        writeln!( self.file, "{}{{", self.indentation)?;
        self.indentation.push_str( &self.indent_str );
        Ok(())
    }
    
    fn end_scope(&mut self) -> Result<(), GMError> {
        let len = self.indentation.len();
        if len == 0 {
            gm_error!( "Called end_scope without corresponding begin_scope." )
        } else {
            self.indentation.truncate( len - self.indent_str.len() );
            writeln!( self.file, "{}}}", self.indentation )?;
            Ok(())
        }
    }
    
    fn writeln(&mut self, string: String) -> std::io::Result<()> {
        writeln!( self.file, "{}{}", self.indentation, string )
    }

    #[allow(dead_code)]
    fn write(&mut self, string: String) -> std::io::Result<()> {
        write!( self.file, "{}{}", self.indentation, string )
    }

    fn newline( &mut self ) -> std::io::Result<()> {
        writeln!( self.file )
    }

    fn push_type_var_override( &mut self, tv: String ) {
        self.type_var_override.push( tv );
    }

    fn pop_type_var_override( &mut self ) {
        self.type_var_override.pop();
    }
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
        self._typedef = Some( TypeInfoTypedef{ source_type: source_type, ..Default::default() } );
        self
    }
}

fn has_object_parent( struct_info: &TypeInfoStruct, type_info_store: &TypeInfoStore ) -> bool {
    if let Some( ref parent ) = struct_info.parent {
        if parent == "Object" {
            return true;
        }

        if let Some( ref parent_type ) = type_info_store.get( parent ) {
            if let Some( ref parent_struct ) = parent_type._struct {
                return has_object_parent( &parent_struct, type_info_store );
            }
        }
    }

    return false;
}

fn get_source_file( entity: &Entity ) -> Option<TypeInfoSource> {
    match entity.get_location() {
        Some( location ) => {
            if let Some( file_location ) = location.get_file_location().file {
                Some( TypeInfoSource {
                    file_path: file_location.get_path().file_name().unwrap().to_str().unwrap().to_string(),
                    is_system_file: location.is_in_system_header()
                } )
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

fn from_entity_structdecl( context: &mut ParseContext, entity: &Entity ) -> Result<(), GMError> {
    if !entity.is_definition() { return gm_info!( "not definition" ); }

    if let Some(entity) = entity.get_template() {
        return from_entity( context, &entity );
    }

    match ( entity.get_name(), entity.get_type() ) {
        ( Some( _ ), Some( type_def ) ) => {
            let mut type_info = TypeInfo::new( sanitize_type_name(&type_def) ).make_struct( &None, get_struct_kind_from_entity_kind( entity.get_kind() ) );
            type_info.source_file = get_source_file( entity );
            context.reserve_type_info_name( type_info.name.clone() );

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
                            match from_entity_fielddecl( context, &child, entity.get_type().as_ref() ) {
                                Ok( type_field ) => { type_info_struct.fields.push( type_field ); },
                                Err( err ) => { println!( "{}", err )},
                            }
                        },
                        Method => {
                            match from_entity_funcdecl( context, &child ) {
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
            context.store_type_info( type_info );
            Ok( () )
        },
        ( None, Some(_) ) => gm_error!( "Couldn't generate a TypeInfo from this StructDecl (missing name).".to_string() ),
        ( Some(_), None ) => gm_error!( "Couldn't generate a TypeInfo from this StructDecl (missing type).".to_string() ),
        ( None, None )    => gm_error!( "Couldn't generate a TypeInfo from this StructDecl.".to_string() ),
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
        if type_def.get_typedef_name().is_none() { // @TODO: This will fail if the typedef is a type alias of a partially specialized template
            let mut tas: Vec<TypeInfo> = vec!();
            for arg in template_args.iter().filter_map( |a| *a ) {
                let mut thing = TypeInfo::new( "" ).make_field( "", 0 );
                let true_type = make_field_from_type( thing._field.as_mut().unwrap(), &arg )?;
                thing.name = sanitize_type_name( &true_type );
                tas.push( thing );
            }
            field_info.templates = Some( tas );
        }
    }

    Ok( type_def )
}

fn from_entity_fielddecl( context: &mut ParseContext, entity: &Entity, parent_type: Option<&Type> ) -> Result<TypeInfo, GMError> {
    match ( entity.get_name(), entity.get_type() ) {
        ( Some( name ), Some( type_def ) ) => {
            // type name filled in later
            let mut type_info = TypeInfo::new( "" ).make_field( &name, if let Some(parent_type) = parent_type { ( parent_type.get_offsetof( &name ).unwrap_or(0) as u32 ) / 8 } else { 0 } );
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

                if !context.store.has( &type_info.name ) {
                    if let Some( declaration ) = type_def.get_declaration() {
                        if let Err(e) = from_entity( context, &declaration ) {
                            println!( "Can't resolve dependency: {}", e );
                        }
                    }
                }
            }

            let type_info = type_info;
            Ok( type_info )
        },
        _ => Err( GMError::error( "Couldn't generate Field from this FieldDecl.".to_string() ) ),
    }
}

fn from_entity_enumdecl( context: &mut ParseContext, entity: &Entity ) -> Result<(), GMError> {
    if !entity.is_definition() { return gm_info!( "not definition" ); }
    match entity.get_type() {
        Some( type_def ) => {
            let mut type_info = TypeInfo::new( type_def.get_display_name() ).make_enum( &entity.get_enum_underlying_type().unwrap().get_display_name() );
            type_info.source_file = get_source_file( entity );

            {
                let mut type_enum = type_info._enum.as_mut().unwrap();
                type_enum.is_scoped = entity.is_scoped();
                for child in entity.get_children() {
                    type_enum.enum_values.insert( child.get_name().unwrap().clone(), child.get_enum_constant_value().unwrap() );
                }
            }

            let type_info = type_info;
            context.store_type_info(type_info);
            Ok(())
        },
        _ => gm_error!( "Couldn't generate TypeInfo from this EnumDecl." ),
    }
}

fn from_entity_freefunction( context: &mut ParseContext, entity: &Entity ) -> Result<(), GMError> {
    let func_info = from_entity_funcdecl( context, entity )?;
    context.store_type_info(func_info);
    Ok(())
}

fn from_entity_funcdecl( context: &mut ParseContext, entity: &Entity ) -> Result<TypeInfo, GMError> {
    if let Some( _ ) = entity.get_template() { return gm_info!( "we don't handle template func." ); }

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
                    match from_entity_fielddecl( context, child, None ) {
                        Ok( param_type ) => type_func.parameters.push( param_type ),
                        Err( e ) => println!("Something happend while parsing template param: {}", e)
                    };
                }
            }
            Ok( type_info )
        }
        _ => gm_info!( "Missing name or type for func decl.".to_string() )
    }
}

fn get_full_name( entity: &Entity ) -> String {
    let mut entity = entity.clone();
    let mut name = String::new();
    while entity.get_semantic_parent().is_some() {
        if name.len() == 0 {
            name = entity.get_name().unwrap().clone();
        } else {
            name = format!( "{}::{}", entity.get_name().unwrap(), name );
        }
        entity = entity.get_semantic_parent().unwrap().clone();
    }

    name
}

fn from_entity_classtemplate( context: &mut ParseContext, entity: &Entity ) -> Result<(), GMError> {
    if !entity.is_definition() {
        return Err( GMError::info( "Not definition.".to_string() ) );
    }

    
    let name = get_full_name( entity );
    // println!("{:?}", entity);
    let mut type_info = TypeInfo::new( name ).make_template();
    type_info.source_file = get_source_file( entity );

    let type_info = type_info;

    context.store_type_info( type_info );
    Ok( () )
}

fn from_entity_typedefdecl( context: &mut ParseContext, entity: &Entity ) -> Result<(), GMError> {
    let type_def = entity.get_type().unwrap();
    let underlying_type = type_def.get_canonical_type();
    let underlying_type_name = sanitize_type_name( &underlying_type );
    let type_name = sanitize_type_name( &type_def );

    let mut type_info = TypeInfo::new( type_name ).make_typedef( underlying_type_name );
    type_info.source_file = get_source_file( entity );

    if let Some( declaration ) = underlying_type.get_declaration() {
        if let Some( definition ) = declaration.get_definition() {
            let type_info_field = from_entity_fielddecl( context, &definition, None )?;
            if type_info_field._field != Some( TypeInfoField::default() ) {
                type_info._typedef.as_mut().unwrap().field = Some( Box::new( type_info_field ) );
            }
        }
    } else {
        let mut type_info_field = TypeInfo::new( "" ).make_field( "", 0 );
        {
            let mut field_info = type_info_field._field.as_mut().unwrap();
            let type_def = make_field_from_type( &mut field_info, &underlying_type )?;
            type_info_field.name = sanitize_type_name( &underlying_type );
            if !context.store.has( &type_info_field.name ) {
                if let Some( declaration ) = type_def.get_declaration() {
                    if let Err(e) = from_entity( context, &declaration ) {
                        println!( "Can't resolve dependency: {}", e );
                    }
                }
            }
        }
        if type_info_field._field != Some( TypeInfoField::default() ) {
            type_info._typedef.as_mut().unwrap().field = Some( Box::new( type_info_field ) );
        }
    }

    context.store_type_info(type_info);
    return Ok( () );

    // gm_info!( "Found typedef that's not a basic type {} -> {}.", underlying_type, type_name )
}

fn from_entity( context: &mut ParseContext, entity: &Entity ) -> Result<(), GMError> {
    match entity.get_kind() {
        EntityKind::StructDecl    => from_entity_structdecl( context, entity ),
        EntityKind::ClassDecl     => from_entity_structdecl( context, entity ),
        EntityKind::EnumDecl      => from_entity_enumdecl( context, entity ),
        EntityKind::FunctionDecl  => from_entity_freefunction( context, entity ),
        EntityKind::ClassTemplate => from_entity_classtemplate( context, entity ),
        EntityKind::TypedefDecl   => from_entity_typedefdecl( context, entity ),
        EntityKind::TypeAliasDecl => from_entity_typedefdecl(context, entity),
        kind => gm_info!( "Unhandled entity kind: {:?}", kind),
    }
}

fn write_header( type_info_store: &TypeInfoStore, options: &Options ) -> Result<bool, GMError> {
    let mut file = File::create( {
        use std::path::PathBuf;
        let mut pathbuf = PathBuf::new();
        if let Some(ref dir) = options.output_directory { pathbuf.push(dir); }
        pathbuf.push("type_db.h");
        pathbuf
    } )?;

    writeln!( file, "#pragma once" )?;
    writeln!( file, "#include \"types.h\"")?;
    writeln!( file )?;

    // system includes
    {
        let mut includes: HashSet<&str> = HashSet::new();

        for type_info in type_info_store.data.iter() {
            if let Some( ref source_file ) = type_info.source_file {
                if source_file.is_system_file {
                    includes.insert( &source_file.file_path );
                }
            }
        }

        for include in includes.iter() {
            writeln!( file, "#include <{}>", include )?;
        }
        writeln!( file )?;
    }

    writeln!( file, "{};", get_register_types_header() )?;
    writeln!( file )?;

    for type_info in type_info_store.data.iter().filter( |t| t._enum.is_some() ) {
        let enum_type = type_info._enum.as_ref().unwrap();
        if enum_type.is_scoped {
            writeln!( file, "enum class {} : {};", type_info.name, enum_type.underlying_type )?;
        }
    }

    writeln!( file )?;

    for type_info in type_info_store.data.iter().filter( |t| t._struct.is_some() ) {
        match type_info._struct.as_ref().unwrap().kind {
            TypeInfoStructKind::Struct => writeln!( file, "struct {};", type_info.name )?,
            TypeInfoStructKind::Class => writeln!( file, "class {};", type_info.name )?,
        }
    }

    writeln!( file )?;

    writeln!( file, "enum class LocalTypeId : uint32_t")?;
    writeln!( file, "{{")?;
    for type_info in type_info_store.data.iter() {
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
    for type_info in type_info_store.data.iter()
                    .filter( |t| t._struct.is_some() )
                    .filter( |t| has_object_parent( t._struct.as_ref().unwrap(), type_info_store ) ) {
        writeln!( file, "    {},", type_info.name )?;
    }
    writeln!( file, "}};" )?;

    writeln!( file )?;

    for type_info in type_info_store.data.iter() {
        use TypeInfoType::*;
        match get_type_info_type( &type_info ) {
            Scalar(_)  => { writeln!( file, "template<> constexpr TypeId type_id<{}>() {{ return {{ 0, (uint32_t)LocalTypeId::{} }}; }}", type_info.name, get_type_id(&type_info) )?; }
            Typedef(_) => { continue; }
            Struct(_)  => { writeln!( file, "template<> constexpr TypeId type_id<{}>() {{ return {{ 0, (uint32_t)LocalTypeId::{} }}; }}", type_info.name, get_type_id(&type_info) )?; }
            Enum( enum_info )    => {
                if enum_info.is_scoped {
                    writeln!( file, "template<> constexpr TypeId type_id<{}>() {{ return {{ 0, (uint32_t)LocalTypeId::{} }}; }}", type_info.name, get_type_id(&type_info) )?; 
                }
                else {
                    continue;
                }
            }
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

    if field.is_const   { result.push( "FieldInfoModifier::CONSTANT" ); }
    if field.is_ptr     { result.push( "FieldInfoModifier::POINTER" ); }
    if field.is_private { result.push( "FieldInfoModifier::PRIVATE" ); }
    if field.is_ref     { result.push( "FieldInfoModifier::REFERENCE" ); }


    match result.is_empty() {
        true  => String::from( "FieldInfoModifier::NONE" ),
        false => result.join( " | " )
    }
}

fn write_struct_implementation( type_info_store: &TypeInfoStore, file: &mut File, type_info: &TypeInfo ) -> Result<bool, GMError> {
    let struct_type = type_info._struct.as_ref().unwrap();
    // Write constructors if needed
    if has_object_parent( struct_type, type_info_store ) {
        let parent = struct_type.parent.as_ref().unwrap();
        writeln!( file, "{struct_name}::{struct_name}() : {parent_name}( type_id<{struct_name}>() ) {{}}", struct_name = type_info.name, parent_name = parent )?;
        writeln!( file, "{struct_name}::{struct_name}( TypeId _type_id ) : {parent_name}( _type_id ) {{}}", struct_name = type_info.name, parent_name = parent )?;
    }

    writeln!( file )?;

    Ok( true )
}

fn get_register_types_header() -> &'static str {
    "void register_types( TypeInfo& (*alloc_type) ( void* ), void* alloc_type_param, void* (*alloc_data) ( void*, uint32_t ), void* alloc_data_param )"
}

fn get_field_var( type_name: &String, field_type: &Option<TypeInfoField> ) -> String {
    let type_var = get_type_var( type_name );
    if let &Some( ref field_info ) = field_type {
        format!( "{}_{}", type_var, field_info.field_name )
    } else {
        type_var
    }
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

fn sanitize_type_var( type_name: &String ) -> String {
    type_name.replace("[", "_")
             .replace("]", "_")
}

fn get_type_id( type_info: &TypeInfo ) -> String {
    let type_name = &type_info.name;
    format!( "{}_id", type_name.replace("::", "_").replace( " ", "_" ) )
}

fn write_type_instantiation( context: &mut ExportContext, type_info: &TypeInfo ) -> std::io::Result<()> {
    let type_name = &type_info.name;
    let type_var  = get_field_var( &type_name, &type_info._field );
    
    use TypeInfoType::*;
    match get_type_info_type( &type_info ) {
        Scalar(_)           => gm_writeln!( context, "auto& {type} = alloc_type_short( TypeInfoType::Scalar );",   type=type_var ),
        Typedef( typedef )  => {
            if typedef.field.is_some() {
                gm_writeln!( context, "auto& {} = alloc_type_short( TypeInfoType::Typedef );", type_var )
            } else {
                gm_writeln!( context, "auto& {type} = alloc_type_short( {source}.type );", type=type_var, source=get_type_var( &typedef.source_type ) )
            }
        },
        Enum(_)             => gm_writeln!( context, "auto& {type} = alloc_type_short( TypeInfoType::Enum );",     type=type_var ),
        Template(_)         => gm_writeln!( context, "auto& {type} = alloc_type_short( TypeInfoType::TemplateDef );", type=type_var ),
        Struct(_)           => gm_writeln!( context, "auto& {type} = alloc_type_short( TypeInfoType::Struct );",   type=type_var ),
        Func(_)             => Ok(()), // @TODO: Bring this back when we figure out how to handle overload properly: gm_writeln!( context, "auto& {type} = alloc_type( alloc_type_param ); type_set_type( {type}, TypeInfoType::Function );", type=type_var ),
        Field(_)            => Ok(()),
        None                => Ok(()),
    }
}

fn write_type_implementation( context: &mut ExportContext, type_info_store: &TypeInfoStore, template_instances: &HashMap<String, Vec<Vec<TypeInfo>>>, type_info: &TypeInfo, indent_count: usize ) -> Result<bool, GMError> {
    let type_name = &type_info.name;
    let type_var  = match context.type_var_override.last() {
        Option::None => get_field_var( &type_name, &type_info._field ),
        Some( otv ) => otv.clone(),
    };
    let indent    = " ".repeat( indent_count * 4 );

    use TypeInfoType::*;
    match get_type_info_type( &type_info ) {

// Impl Scalar
        Scalar( scalar_type ) => {
            use ScalarInfo::*;
            let scalar_type_name = match scalar_type.scalar_type {
                INT => "INT",
                UINT => "UINT",
                FLOAT => "FLOAT",
                BOOL => "BOOL",
                CHAR => "CHAR",
            };

            gm_writeln!( context, "type_set_name( {}, copy_string( \"{}\" ) );", type_var, type_name )?;
            gm_writeln!( context, "type_set_id( {}, type_id<{}>() );", type_var, type_info.name )?;
            gm_writeln!( context, "scalar_set_size( {}.scalar_info, sizeof( {} ) );", type_var, type_info.name )?;
            gm_writeln!( context, "scalar_set_type( {}.scalar_info, ScalarInfoType::{} );", type_var, scalar_type_name )?;
            gm_writeln!( context )?;
        }

// Impl Enum
        Enum( enum_type ) => {
            gm_writeln!( context, "type_set_name( {}, copy_string( \"{}\" ) );", type_var, type_name )?;
            if enum_type.is_scoped {
                gm_writeln!( context, "type_set_id( {}, type_id<{}>() );", type_var, type_info.name )?;
            } else {
                gm_writeln!( context, "type_set_id( {}, {{ 0, (uint32_t)LocalTypeId::{} }} );", type_var, get_type_id( &type_info ) )?;
            }
            gm_writeln!( context, "enum_set_underlying_type( {}.enum_info, &type_{} );", type_var, enum_type.underlying_type.replace("int", "i32").replace("ushort", "u16") )?;

            if !enum_type.enum_values.is_empty() {
                gm_begin_scope!( context )?;
                gm_writeln!( context, "auto values = (EnumValue*)alloc_data( alloc_data_param, sizeof(EnumValue) * {} );", enum_type.enum_values.len() )?;
                let mut val = 0;
                for ( name, &(value, _) ) in &enum_type.enum_values {
                    gm_writeln!( context, "values[{}] = {{ copy_string( \"{}\" ), {} }};", val, name, value )?;
                    val = val + 1;
                }
                gm_writeln!( context, "enum_set_values( {}.enum_info, values, {} );", type_var, enum_type.enum_values.len() )?;
                gm_end_scope!( context )?;
            }

            context.newline()?;
        }

// Impl Template
        Template( _template_type ) => {
            gm_writeln!( context, "type_set_name( {}, copy_string( \"{}\" ) );", type_var, type_name )?;
            gm_writeln!( context, "type_set_id( {}, {{ 0, (uint32_t)LocalTypeId::{} }} );", type_var, get_type_id( &type_info ) )?;
            if let Some(instances) = template_instances.get( type_name ) {
                let instances_var_name = format!( "{}_instances", type_var );
                gm_writeln!( context, "auto {} = (TypeInfo*)alloc_data( alloc_data_param, sizeof(TypeInfo) * {} );", instances_var_name, instances.len() )?;
                gm_writeln!( context, "template_set_instances( {}, {}, {} );", type_var, instances_var_name, instances.len() )?;
                for (inst_index, instance) in instances.iter().enumerate() {
                    let instance_var_name = format!( "{}[{}]", instances_var_name, inst_index );
                    gm_writeln!( context, "type_set_type( {}, TypeInfoType::TemplateInst );", instance_var_name )?;
                    gm_writeln!( context, "type_set_name( {}, {}.name );", instance_var_name, type_var )?;
                    gm_writeln!( context, "type_set_id( {}, {}.type_id );", instance_var_name, type_var )?;
                    gm_writeln!( context, 
                      "template_instance_set_params( &{}.template_inst_info, (TemplateParam*)alloc_data( alloc_data_param, sizeof(TemplateParam) * {} ), {} );", 
                      instance_var_name, instance.len(), instance.len() )?;
                    gm_writeln!( context, "{}.template_inst_info.definition = &{};", instance_var_name, &type_var )?;
                    gm_writeln!( context, "{}.template_inst_info.definition = &{};", instance_var_name, &type_var )?;
                    for (param_index, param) in instance.iter().enumerate() {
                        context.push_type_var_override( format!( "{}.template_inst_info.params[{}].info", instance_var_name, param_index ) );
                        write_type_implementation( context, type_info_store, template_instances, &param, indent_count + 3 )?;
                        context.pop_type_var_override();
                    }
                    if inst_index < instances.len()-1 { gm_writeln!( context )?; }
                }
            }
            gm_writeln!( context )?;
        }

// Impl Struct
        Struct( struct_type ) => {
            gm_writeln!( context, "type_set_name( {}, copy_string( \"{}\" ) );", type_var, type_name )?;
            gm_writeln!( context, "type_set_id( {}, type_id<{}>() );", type_var, type_info.name )?;
            gm_writeln!( context, "struct_set_size( {}, sizeof( {} ) );", type_var, type_info.name )?;
            if let Some( ref parent ) = struct_type.parent {
                gm_writeln!( context, "struct_set_parent( {}, &{} );", type_var, get_type_var( &parent ) )?;
            }

            if !struct_type.fields.is_empty() {
                let instances_var_name = format!( "{}_fields", type_var );
                gm_writeln!( context, "auto {} = (FieldInfo*)alloc_data( alloc_data_param, sizeof(FieldInfo) * {} );", instances_var_name, struct_type.fields.len() )?;
                let mut idx = 0;
                for field in &struct_type.fields {
                    context.push_type_var_override( format!("{}[{}]", instances_var_name, idx) );
                    write_type_implementation( context, type_info_store, template_instances, field, indent_count+2 )?;
                    context.pop_type_var_override();
                    idx = idx + 1;
                }
                gm_writeln!(context, "struct_set_fields( {}, {}, {} );", type_var, instances_var_name, struct_type.fields.len() )?;
            }

            if !struct_type.functions.is_empty() {
                let functions_var_name = format!( "{}_functions", type_var );
                gm_writeln!( context,
                  "auto {} = (TypeInfo*)alloc_data( alloc_data_param, sizeof(TypeInfo) * {} );",
                  functions_var_name, struct_type.functions.len() )?;
                for (idx, func) in struct_type.functions.iter().enumerate() {
                    gm_begin_scope!( context )?;
                    context.push_type_var_override( format!("{}[{}]", functions_var_name, idx) );
                    write_type_implementation( context, type_info_store, template_instances, func, indent_count+2 )?;
                    context.pop_type_var_override();
                    gm_end_scope!( context )?;
                }
                gm_writeln!( context, "struct_set_functions( {}, {}, {} );", type_var, functions_var_name, struct_type.functions.len() )?;
            }
            writeln!( context.file )?;
        }

// Impl Func
        Func( func_type ) => {
            let func_name = &type_info.name;

            if context.type_var_override.is_empty() {
                gm_begin_scope!( context )?;
                gm_writeln!( context, "auto& {type} = alloc_type( alloc_type_param ); type_set_type( {type}, TypeInfoType::Function );", type=type_var )?;
            } else {
                write_type_instantiation( context, type_info )?;
            }

            gm_writeln!( context, "type_set_name( {}, copy_string( \"{}\" ) );", type_var, func_name )?;
            if let Some( ref return_type ) = func_type.return_type {
                let return_type_var = format!("{}_return_type", sanitize_type_var( &type_var ));
                context.push_type_var_override( return_type_var.clone() );
                gm_writeln!( context, "FieldInfo {};", return_type_var )?;
                let return_type = &return_type;
                write_type_implementation( context, type_info_store, template_instances, return_type, indent_count+1 )?;
                gm_writeln!( context, "func_set_return_type( {}, {} );", type_var, return_type_var )?;
                context.pop_type_var_override();
            }

            if !func_type.parameters.is_empty() {
                let func_parameters_type_var = format!( "{}_params", sanitize_type_var( &type_var ) );
                gm_writeln!( context, "auto* {} = (FieldInfo*)alloc_data( alloc_data_param, sizeof(FieldInfo) * {} );", func_parameters_type_var, func_type.parameters.len() )?;
                let mut idx = 0;
                for param in &func_type.parameters {
                    gm_begin_scope!( context )?;
                    context.push_type_var_override( format!("{}[{}]", func_parameters_type_var, idx ) );
                    write_type_implementation( context, type_info_store, template_instances, param, indent_count+3 )?;
                    context.pop_type_var_override();
                    gm_end_scope!( context )?;
                    idx = idx + 1;
                }
                gm_writeln!( context, "func_set_parameters( {}, {}, {} );", type_var, func_parameters_type_var, func_type.parameters.len() )?;
            }

            if context.type_var_override.is_empty() { gm_end_scope!( context )?; }
            gm_writeln!( context )?;
        }

// Impl Typedef
        Typedef( typedef_type ) => {
            let source_type_var = get_type_var( &typedef_type.source_type );
            if let Some( ref field ) = typedef_type.field {
                gm_writeln!( context, "type_set_name( {}, copy_string( \"{}\" ) );", type_var, type_name )?;
                gm_writeln!( context, "type_set_id( {}, {{ 0, (uint32_t)LocalTypeId::{} }} );", type_var, get_type_id( &type_info ) )?;
                context.push_type_var_override( format!("{}.typedef_info.info", type_var ) );
                write_type_implementation( context, type_info_store, template_instances, &*field, indent_count+3 )?;
                context.pop_type_var_override();
            } else {
                gm_writeln!( context, "{} = {};", type_var, source_type_var )?;
                gm_writeln!( context, "type_set_name( {}, copy_string( \"{}\" ) );", type_var, type_name )?;
                gm_writeln!( context, "type_set_id( {}, {{ 0, (uint32_t)LocalTypeId::{} }} );", type_var, get_type_id( &type_info ) )?;
            }
            gm_writeln!( context )?;
        }

// Impl Field
        Field( field_type ) => {
            if context.type_var_override.is_empty() { writeln!( context.file, "{}FieldInfo {};", indent, type_var )?; };
            if field_type.field_name.len() > 0 {
                gm_writeln!( context, "field_set_name( {}, copy_string( \"{}\" ) );", type_var, field_type.field_name)?;
            }
            if field_type.offset != 0 {
                gm_writeln!( context, "field_set_offset( {}, {} );", type_var, field_type.offset )?;
            }
            gm_writeln!( context, "field_set_modifiers( {}, (FieldInfoModifier) ({}) );", type_var, build_modifier_string( &field_type ) )?;

            if let Some( registered_type ) = type_info_store.get( &type_info.name ) {
                if let Some( ref template_args ) = field_type.templates {
                    let template_src_type_var = get_type_var( &type_info.name );
                    let template_src_instances = &template_instances[&type_info.name];
                    // find instance index
                    let mut inst_index = 0;
                    for inst in template_src_instances.iter() {
                        if inst == template_args {
                            break;
                        }
                        inst_index += 1;
                    }
                    let inst_index = inst_index;

                    gm_writeln!( context, 
                      "field_set_type( {}, &{}.template_def_info.instances[{}] );",
                      type_var, template_src_type_var, inst_index )?;
                } else {
                    gm_writeln!( context, "field_set_type( {}, &{} );", type_var, get_type_var( &registered_type.name ) )?;
                }
            }
        }

        None => {}
    };

    Ok(true)
}

fn write_implementation( type_info_store: &TypeInfoStore, template_instances: &HashMap<String, Vec<Vec<TypeInfo>>>, options: &Options ) -> Result<bool, GMError> {
    let file = File::create( {
        use std::path::PathBuf;
        let mut pathbuf = PathBuf::new();
        if let Some(ref dir) = options.output_directory { pathbuf.push(dir); }
        pathbuf.push("type_db.cpp");
        pathbuf
    } )?;
    let mut context = ExportContext::new( file, "    ".to_string() );

    // includes
    {
        let mut includes: HashSet<&str> = HashSet::new();

        for type_info in type_info_store.data.iter() {
            if let Some( ref source_file ) = type_info.source_file {
                if !source_file.is_system_file {
                    includes.insert( &source_file.file_path );
                }
            }
        }

        let dependencies = &[ "type_db.h" ];
        for dep in dependencies {
            writeln!( context.file, "#include \"{}\"", dep )?;
            includes.remove( dep );
        }
        includes.remove( "types.h" );

        writeln!( context.file )?;

        for include in includes.iter() {
            writeln!( context.file, "#include \"{}\"", include )?;
        }
        writeln!( context.file )?;
    }

    gm_writeln!( context, "{}", get_register_types_header() )?;
    gm_begin_scope!( context )?;

    gm_writeln!( context, "auto copy_string = [&]( const char* str ) -> const char* {{
        auto len = strlen( str );
        if( len == 0 )
            return nullptr;
        char* result = (char*)alloc_data( alloc_data_param, (uint32_t)len+1 );
        strcpy_s( result, len+1, str );
        return result;
    }};")?;

    gm_writeln!( context, "auto alloc_type_short = [&]( TypeInfoType type ) -> TypeInfo& {{
        auto& type_info = alloc_type( alloc_type_param );
        type_set_type( type_info, type );
        return type_info;
    }};")?;

    gm_writeln!( context )?;

    for type_info in type_info_store.data.iter() {
        write_type_instantiation( &mut context, type_info )?;
    }

    gm_writeln!( context )?;

    for type_info in type_info_store.data.iter() {
        write_type_implementation( &mut context, type_info_store, template_instances, type_info, 1 )?;
    }

    gm_end_scope!( context )?;
    gm_writeln!( context )?;

    for type_info in type_info_store.data.iter().filter( |t| t._struct.is_some() && has_object_parent( t._struct.as_ref().unwrap(), type_info_store ) ) {
        write_struct_implementation( type_info_store, &mut context.file, type_info )?;
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

fn parse_translation_unit( tu: &TranslationUnit, options: &Options ) -> ParseContext {
    let mut context = ParseContext::default();
    for built_in in get_built_in_types() {
        context.store_type_info( built_in.clone() );
    }

    for entity in tu.get_entity().get_children().iter().filter( |e| !e.is_in_system_header() ) {
        match from_entity( &mut context, &entity ) {
            Ok( _ ) =>  {
            },
            Err( error ) => {
                if options.verbose {
                    println!( "({}): {}", get_source_file( &entity ).unwrap_or_else(|| TypeInfoSource{ file_path: String::from("NoFile"), is_system_file: false }).file_path, error ); 
                }
            },
        };
    }

    context
}

fn main() {
    let start = Instant::now();

    let mut options = Options::default();

    {
        let mut ap = ArgumentParser::new();
        ap.set_description("Generate metadata files for my C++ GameEngine.");
        ap.refer(&mut options.verbose)
            .add_option(&["-v", "--verbose"], StoreTrue, "Be verbose");
        ap.refer(&mut options.no_output)
            .add_option(&["--no-output"], StoreTrue, "Don't output type_db.");
        ap.refer(&mut options.no_report)
            .add_option(&["--no-report"], StoreTrue, "Don't output type report.");
        ap.refer(&mut options.output_directory)
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

    let mainh_path = PathBuf::from("main.h");
    let files = entries.filter(|x| x.file_type().unwrap().is_file())
                        .map(|x| x.path() )
                        .filter(|x| x.extension().is_some())
                        .filter(|x| x.extension().unwrap() == "h")
                        .filter(|x| x == &mainh_path)
                        .collect();

    match generate_main_file( &files ) {
        Err( err ) => println!( "ERROR: {}", err ),
        Ok(_) => {},
    };

    let arguments = get_clang_arguments( &options.input_directories, &options.additional_include_directories );

    let clang = Clang::new().unwrap();
    let index = Index::new(&clang, false, true);

    let parse_context = match index.parser( &"main.h" ).arguments( &arguments ).parse() {
        Ok( tu )   => parse_translation_unit( &tu, &options ),
        Err( err ) => {
            println!( "ERROR: {}", err );
            ParseContext::default()
        }
    };
 
    fn explore_fields_rec( template_instances: &mut HashMap<String, Vec<Vec<TypeInfo>>>, type_info: &TypeInfo ) {
        let field_info = type_info._field.as_ref().unwrap();
        if let Some( ref template_args ) = field_info.templates {
            let template_type = &type_info.name;
            {
                let mut instances = template_instances.entry( template_type.clone() ).or_insert_with(|| vec!() );
                if !instances.iter().any( |inst| inst == template_args ) {
                    instances.push( template_args.clone() );
                }
            }
            for arg in template_args.iter() {
                explore_fields_rec( template_instances, &arg );
            }
        }
    }
    
    let mut template_instances: HashMap<String, Vec<Vec<TypeInfo>>> = HashMap::new();
    for type_info in &parse_context.store.data {
        if type_info._struct.is_some() {
            let struct_info = type_info._struct.as_ref().unwrap();
            for field in &struct_info.fields {
                explore_fields_rec( &mut template_instances, &field );
            }
        } else if type_info._typedef.is_some() {
            let typedef_info = type_info._typedef.as_ref().unwrap();
            if let Some( ref field ) = typedef_info.field {
                explore_fields_rec( &mut template_instances, &*field );
            }
        }
    }
    let template_instances = template_instances;

    if !options.no_report {
        for (k, v) in &template_instances {
            println!( "{} instances: ", k );
            for inst in v.iter() {
                for f in inst.iter() {
                    print!( "    {}", f.name );
                }
                println!();
            }
        }

        show_report_types( &parse_context.store.data );
    }

    if !options.no_output {
        match write_header( &parse_context.store, &options ) {
            Ok(_) => {},
            Err( err ) => { println!( "{}", err ); },
        }

        match write_implementation( &parse_context.store, &template_instances, &options ) {
            Ok(_) => {},
            Err( err ) => { println!( "{}", err ); },
        }
    }

    let duration = start.elapsed();
    let nanos = duration.subsec_nanos() as f64;
    let s = (1000.0*1000.0*1000.0 * (duration.as_secs() as f64) + nanos)/(1000.0 * 1000.0 * 1000.0 );
    println!( "Finished in {}s.", s );
}
