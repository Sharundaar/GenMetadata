#include <iostream>
#include <string>
#include <cstring>

#include "object.h"

using namespace std;

int main( int, char** )
{
    for( const auto** typePtr = &s_all_types[0]; *typePtr != nullptr; ++typePtr )
    {
        const auto* type = *typePtr;
        cout << "Type: " << type->name << endl;
        if( type->type == TypeInfo_Type::STRUCT )
        {
            const auto* struct_type = (StructInfo*) type;
            for( auto member : struct_type->fields )
            {
                cout << "\t" << member.offset << ": " << member.type->name << " " << member.name << endl;
            }
        }
    }

    MyStruct myStruct;
    myStruct.number1 = 42;
    myStruct.number2 = 15;

    i8 new_number2 = 3;

    cout << to_string( myStruct.number1 ) << " " << to_string( myStruct.number2 ) << endl;

    const auto& number2_field = myStruct.get_type()->get_field( "number2" );
    number2_field.set<i8>( &myStruct, new_number2 );
    cout << to_string( myStruct.number1 ) << " " << to_string( myStruct.number2 ) << endl;

    cin.ignore();
    return 0;
}