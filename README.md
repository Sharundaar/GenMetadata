# GenMetadata
Software used to generate C++ type metadata.

# Testing
This can be tested by running
    cargo run --release -- cpp --output cpp
    
this will create type_db.h and type_db.cpp in the cpp folder.

Then we can setup cmake :
    cd cpp
    mkdir build
    cd build
    cmake ..
    cd ../..
This will setup cmake with the default generator

We can then run
    cmake --build cpp/build

This should build the c++ project and output the binaries in cpp/bin/Debug/
We can then run cpp/bin/Debug/TypeInfoTest.exe to check that the metadata are generated correctly.

