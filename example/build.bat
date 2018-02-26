@echo off

g++ types.cpp -g -c -o types.obj
g++ type_db.cpp -g -c -o type_db.obj
g++ main.cpp -g -c -o main.obj
g++ main.obj types.obj type_db.obj -g -o TypeInfo.exe
