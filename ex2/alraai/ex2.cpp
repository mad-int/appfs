#include <iostream>

using namespace std;

int main()
{
    string s[] = {
    "#include <iostream>",
    " ",
    "using namespace std;",
    " ",
    "int main()",
    "{",
    "   string s[] = {",
    " ",
    "   char q = 19",
    " ",
    "   for(int i = 0; i < q; i++){",
    "       cout << s[i] << endl;",
    "   }",
    "   for(int j = 0; j < q; j++){",
    "       cout << s[j] << endl;",
    "   }",
    "   for(int k = 8; k < q; k++){",
    "       cout << s[k] << endl;",
    "   }",
    " ",
    "   return 0;",
    "}"};

    char q = 34;

    for(int i = 0; i < 7; i++){
        cout << s[i] << endl;
    }
    for(int j = 0; j < 22; j++){
        cout << s[7] + q  + s[j] + q + ","<< endl;
    }
    for(int k = 8; k < 22; k++){
        cout << s[k] << endl;
    }

    return 0;
}
