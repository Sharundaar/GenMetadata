@echo off

del type_db.h
del type_db.cpp

cd ..
cargo run --release -- example
copy type_db.* example
cd example


g++ types.cpp -g -c -o types.obj -Wall
g++ type_db.cpp -g -c -o type_db.obj -Wall
g++ main.cpp -g -c -o main.obj -Wall
g++ main.obj types.obj type_db.obj -g -o TypeInfo.exe -Wall
