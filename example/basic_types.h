#pragma once

#include <cstdint>
#include <limits>

typedef unsigned int   uint;
typedef unsigned short ushort;
typedef unsigned long  ulong;
typedef unsigned char  uchar;

typedef int8_t    i8;
typedef int16_t   i16;
typedef int32_t   i32;
typedef int64_t   i64;

typedef uint8_t   u8;
typedef uint16_t  u16;
typedef uint32_t  u32;
typedef uint64_t  u64;

typedef float     f32;
typedef double    f64;

template<typename T> const T max_value() { return ( std::numeric_limits<T>::max ) ();    }
template<typename T> const T min_value() { return ( std::numeric_limits<T>::lowest ) (); }
