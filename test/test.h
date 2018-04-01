
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
    TemplateClass<int> cl;
    TemplateStruct<char, 23> ts;
    int i;
};