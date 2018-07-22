extern crate argparse;
use self::argparse::{ArgumentParser, StoreTrue, StoreOption, Collect, List};

#[derive(Default)]
pub struct Options {
    verbose: bool,
    input_directories: Vec<String>,
    additional_include_directories: Vec<String>,
    output_directory: Option<String>,
    no_output: bool,
    no_report: bool,
}

impl Options {
    pub fn is_verbose( &self ) -> bool {
        self.verbose
    }

    pub fn get_input_directories( &self ) -> &Vec<String> {
        &self.input_directories
    }

    pub fn get_additional_include_directories( &self ) -> &Vec<String> {
        &self.additional_include_directories
    }

    pub fn get_output_directory( &self ) -> &str {
        match self.output_directory {
            Some( ref output ) => &output,
            None => ""
        }
    }

    pub fn is_no_output( &self ) -> bool {
        self.no_output
    }

    pub fn is_no_report( &self ) -> bool {
        self.no_report
    }
}

pub fn parse_options() -> Options {
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
    options
}