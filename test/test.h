
#include <string>

typedef unsigned int u32;

template<typename T>
class TemplateClass
{
    T data;
};

template<typename T, int I>
struct TemplateStruct
{
    T data[I];
};

struct [[SomeAttribute]] SomeStruct
{
    TemplateClass<const u32*> cl;
    TemplateStruct<char, 23> ts;
    std::string str;
    u32 i;
};

class SomeClass
{
    char c;
    void some_function();
    void some_function2( u32 a );
    u32  some_function3( u32 b );
    int  some_error_function( int a );
};
