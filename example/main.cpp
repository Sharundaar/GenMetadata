#include <iostream>
#include <string>
#include <cstring>

#include "object.h"
#include "object_pools.h"

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

    init_pool<MyStruct>();

    auto struct_pool = get_pool<MyStruct>();
    auto myStruct = struct_pool->Instantiate();

    cout << myStruct->number1 << " " << to_string( myStruct->number2 ) << endl;

    struct_pool->Destroy( myStruct );
    delete_pool<MyStruct>();

    show_pool_report();

    cin.ignore();
    return 0;
}