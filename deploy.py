import subprocess
import sys
from termcolor import colored, cprint
from shutil import copy2

gen_metadata_build_command = "cargo build --release"
gen_metadata_exec_path = "target/release/gen-metadata.exe"
gen_metadata_dest_path = "../Externals/GenMetadata"

sys.stdout.write( 'Compiling source...' )
result = subprocess.run( gen_metadata_build_command, stdout=subprocess.PIPE, stderr=subprocess.PIPE )
if result.returncode != 0:
    sys.stdout.write( colored( ' Failure\n', 'red' ) )
    print( result.stderr.decode('utf-8') )
    sys.exit( result.returncode )

sys.stdout.write( colored(' Success\n', 'green' ) )

sys.stdout.write( 'Copy outputfile to {}...'.format( gen_metadata_dest_path ) )
try:
    copy2( gen_metadata_exec_path, gen_metadata_dest_path )
except IOError as e:
    sys.stdout.write( colored(' Failure\n', 'red' ) )
    print( e )
    sys.exit( 1 )

sys.stdout.write( colored(' Success\n', 'green' ) )

sys.exit( 0 )
