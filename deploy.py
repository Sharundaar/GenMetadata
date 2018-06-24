import subprocess
import sys
import os
from termcolor import colored, cprint
from shutil import copy2
from glob import iglob

gen_metadata_rust_build_command = "cargo build --release"
gen_metadata_rust_exec_path = "target/release/gen-metadata.exe"
gen_metadata_libclang_path  = "libclang.dll"
gen_metadata_rust_run_command = "{} cpp --output cpp".format(gen_metadata_rust_exec_path)
gen_metadata_cpp_build_command = "cmake --build build --target ALL_BUILD --config Debug"
gen_metadata_cpp_exec_path = "cpp/bin/Debug/TypeInfo.lib"
gen_metadata_cpp_include_list = [
    "cpp/types.h"
]

gen_metadata_cpp_test_expected_output = [ 'cpp/type_db.cpp', 'cpp/type_db.h' ]

gen_metadata_dest_path = "../Externals/GenMetadata"

sys.stdout.write( 'Compiling Rust source...' )
sys.stdout.flush()
result = subprocess.run( gen_metadata_rust_build_command, stdout=subprocess.PIPE, stderr=subprocess.PIPE )
if result.returncode != 0:
    sys.stdout.write( colored( ' Failure\n', 'red' ) )
    print( result.stderr.decode('utf-8') )
    sys.exit( result.returncode )

sys.stdout.write( colored(' Success\n', 'green' ) )

sys.stdout.write( 'Testing on CPP...' )
sys.stdout.flush()
result = subprocess.run( gen_metadata_rust_run_command, stdout=subprocess.PIPE, stderr=subprocess.PIPE )
if result.returncode != 0:
    sys.stdout.write( colored( ' Failure\n', 'red' ) )
    print( result.stderr.decode('utf-8') )
    sys.exit( result.returncode )
sys.stdout.write( colored(' Success\n', 'green' ) )

sys.stdout.write( 'Compiling CPP source...' )
sys.stdout.flush()
result = subprocess.run( gen_metadata_cpp_build_command, cwd="cpp", stdout=subprocess.PIPE, stderr=subprocess.PIPE )
if result.returncode != 0:
    sys.stdout.write( colored( ' Failure\n', 'red' ) )
    print( result.stderr.decode('utf-8') )
    sys.exit( result.returncode )

sys.stdout.write( colored(' Success\n', 'green' ) )

sys.stdout.write( 'Copy outputfile to {}...'.format( gen_metadata_dest_path ) )
sys.stdout.flush()
try:
    copy2( gen_metadata_rust_exec_path, gen_metadata_dest_path )
    copy2( gen_metadata_cpp_exec_path, gen_metadata_dest_path )
    copy2( gen_metadata_libclang_path, gen_metadata_dest_path )
    for file in gen_metadata_cpp_include_list:
        copy2( file, os.path.join( gen_metadata_dest_path, "include" ) )
except IOError as e:
    sys.stdout.write( colored(' Failure\n', 'red' ) )
    print( e )
    sys.exit( 1 )

sys.stdout.write( colored(' Success\n', 'green' ) )

sys.exit( 0 )
