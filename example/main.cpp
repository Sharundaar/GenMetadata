#include <iostream>
#include <string>

#include "object.h"

using namespace std;

int main( int, char** )
{
    MyStruct myStruct;
    cout << sizeof(myStruct) << endl;
    cout << myStruct.get_type()->size << endl;

    for( const auto** typePtr = &s_registered_structs[0]; *typePtr != nullptr; ++typePtr )
    {
        const auto* type = *typePtr;
        cout << "Type: " << type->name << endl;
        for( auto member : type->members )
        {
            cout << "\t" << member.member_type->name << " " << member.name << endl;
        }
    }

    cin.ignore();
    return 0;
}