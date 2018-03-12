@echo off

REM del type_db.*
REM cd ..
REM call cargo run --release -- example
REM call copy type_db.* example
REM cd example
REM timeout /t 1

g++ types.cpp -g -c -o types.obj -Wall -std=c++11
g++ type_db.cpp -g -c -o type_db.obj -Wall -std=c++11
g++ basics.cpp -g -c -o basics.obj -Wall -std=c++11
g++ object_pools.cpp -g -c -o object_pools.obj -Wall -std=c++11
g++ main.cpp -g -c -o main.obj -Wall -std=c++11
g++ main.obj types.obj type_db.obj basics.obj object_pools.obj -g -o TypeInfo.exe -Wall -std=c++11
