#include <iostream>
#include <string>

#include "object.h"

using namespace std;

int main( int, char** )
{
    MyStruct myStruct;
    cout << sizeof(myStruct) << endl;
    cout << myStruct.get_type()->size << endl;

    for( const auto** typePtr = &s_all_types[0]; *typePtr != nullptr; ++typePtr )
    {
        const auto* type = *typePtr;
        cout << "Type: " << type->name << endl;
        if( type->type == TypeInfo_Type::STRUCT )
        {
            const auto* struct_type = (StructType*) type;
            for( auto member : struct_type->members )
            {
                cout << "\t" << member.member_type->name << " " << member.name << endl;
            }
        }
    }

    cin.ignore();
    return 0;
}