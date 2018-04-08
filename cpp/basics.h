#pragma once

#include <string>
#include "basic_types.h"

#define INVALID_POOL_INDEX 4294967295

#define assert(Condition, Message) ((!!(Condition)) || _assert(Condition, Message))
#define assert_fmt(Condition, Message, ...) assert(Condition, format(Message, ##__VA_ARGS__))
bool _assert(bool condition, const std::string& msg = "");

#define HAS_FLAG( var, flag ) ((var & flag) == flag )

template<typename T>
void format(std::string& out, const T& val)
{
    out += val;
}

template<> void format<char>   (std::string& out, const char& val);
template<> void format<bool>   (std::string& out, const bool& val);
template<> void format<int>    (std::string& out, const int& val);
template<> void format<float>  (std::string& out, const float& val);
template<> void format<double> (std::string& out, const double& val);
template<> void format<short>  (std::string& out, const short& val);
template<> void format<long>   (std::string& out, const long& val);

template<> void format<unsigned int>   (std::string& out, const unsigned int  & val);
template<> void format<unsigned long>  (std::string& out, const unsigned long & val);

// @Bug: There may be a bug here... out is not used
template<typename T>
const char* format(const char* fmt, std::string& out)
{
    return fmt;
}

template<typename T>
const char* format(const char* fmt, std::string& out, const T& val)
{
    const char* ptr = fmt;
    while(*ptr != '%' && *ptr != '\0')
    {
        out += *ptr;
        ++ptr;
    }

    if(*ptr == '\0') return ptr;
    format(out, val);
    ++ptr;
    return ptr;
}

template<typename U, typename... T>
const char* format(const char* fmt, std::string& out, const U& val, const T&... rest)
{
    fmt = format(fmt, out, val);
    if(*fmt == '\0') return fmt;
    fmt = format(fmt, out, rest... );
    if(*fmt == '\0') return fmt;
    while(*fmt != '\0') out += *(fmt++);
    return fmt;
}

template<typename U, typename... T>
std::string format(const char* fmt, const U& val, const T&... rest)
{
    std::string str;
    format(fmt, str, val, rest... );
    return std::move(str);
}

void print(const std::string& str);

void println();

void println(const std::string& str);

template<typename... T>
void println(const std::string& fmt, const T&... args)
{
    println(format(fmt.c_str(), args...));
}

std::string working_directory();
