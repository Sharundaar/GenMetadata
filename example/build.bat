@echo off

del type_db.*

cd ..
cargo run --release -- example
copy type_db.* example
cd example
timeout /t 1

g++ types.cpp -g -c -o types.obj -Wall
g++ type_db.cpp -g -c -o type_db.obj -Wall
g++ basics.cpp -g -c -o basics.obj -Wall
g++ object_pools.cpp -g -c -o object_pools.obj -Wall
g++ main.cpp -g -c -o main.obj -Wall
g++ main.obj types.obj type_db.obj basics.obj object_pools.obj -g -o TypeInfo.exe -Wall
