#include <iostream>
#include <string>
#include <cstring>

#include "object.h"

using namespace std;

void set_field( Object* obj, std::string field_name, void* new_value )
{
    auto* type = obj->get_type();
    for( auto member : type->members )
    {
        if( member.name == field_name )
        {
            memcpy( ((char*)obj)+(member.offset/8), new_value, member.size);
        }
    }

}

int main( int, char** )
{
    for( const auto** typePtr = &s_all_types[0]; *typePtr != nullptr; ++typePtr )
    {
        const auto* type = *typePtr;
        cout << "Type: " << type->name << endl;
        if( type->type == TypeInfo_Type::STRUCT )
        {
            const auto* struct_type = (StructInfo*) type;
            for( auto member : struct_type->members )
            {
                cout << "\t" << member.offset << ": " << member.member_type->name << " " << member.name << endl;
            }
        }
    }

    MyStruct myStruct;
    myStruct.number1 = 42;
    myStruct.number2 = 15;

    i8 new_number2 = 3;

    cout << to_string( myStruct.number1 ) << " " << to_string( myStruct.number2 ) << endl;

    set_field( &myStruct, "number2", &new_number2 );
    cout << to_string( myStruct.number1 ) << " " << to_string( myStruct.number2 ) << endl;

    cin.ignore();
    return 0;
}