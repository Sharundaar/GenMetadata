#include "basics.h"

#include <iostream>
#include <windows.h>

using namespace std;

void print(const string& str)
{
    cout << str;
}

void println()
{
    cout << endl;
}

void println(const string& str)
{
    cout << str << endl;
}

// @Improvements: create an assert macro that automagically append the file name and line number
//                from where the assert originated to the message
bool _assert(bool condition, const std::string& msg)
{
#ifndef NDEBUG
    if(!condition)
    {
        cerr << msg << endl << endl;
        terminate();
        return false;
    }
#endif
    return true;
}

template<> void format<char>(string& out, const char& val)
{
    out.push_back(val);
}

template<> void format<bool>(string& out, const bool& val)
{
    if(val)
        out += "True";
    else
        out += "False";
}

template<> void format<long> (std::string& out, const long& val)
{
    out += move(to_string(val));
}

template<> void format<int>(string& out, const int& val)
{
    out += move(to_string(val));
}

template<> void format<float>(string& out, const float& val)
{
    out += move(to_string(val));
}

template<> void format<double>(string& out, const double& val)
{
    out += move(to_string(val));
}

template<> void format<short>(string& out, const short& val)
{
    out += move(to_string(val));
}

template<> void format<size_t> (string& out, const size_t& val)
{
    out += move(to_string(val));
}

template<> void format<unsigned long> (string& out, const unsigned long& val)
{
    out += move(to_string(val));
}

template<> void format<unsigned int> (string& out, const unsigned int& val)
{
    out += move(to_string(val));
}

std::string working_directory()
{
    // @Platform: Windows specific
    char buffer[MAX_PATH];
    GetCurrentDirectoryA(MAX_PATH, buffer);
    return string(buffer);
}