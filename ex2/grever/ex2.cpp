
#include <iostream>
#include <fstream>
#include <string>
using namespace std;

int main(int argc, char* argv[] )
{
	char a = 34;
	string l[] = {
		"#include <iostream>",
		"#include <fstream>",
		"#include <string>",
		"using namespace std;",
	    " ",
	    "int main(int argc, char* argv[] )",
	    "{",
	    "	char a = 34;",
	    "	string[] l = {",
	    "	",
	    "	};",
	    "	for(int i = 0; i < 9; i++)",
	    "		cout<<l[i]<<endl;",
	    "	for(int i = 0; i < 19; i++)    ",
	    "		cout<<l[9]+l[9] + a + l[i] + a + ','<<endl;",
	    "	for(int i = 10; i < 19; i++)    ",
	    "		cout<< l[i]<<endl;",
	    "	return 0;",
	    "}",
	};
	for(int i = 0; i < 9; i++)
		cout<<l[i]<<endl;
	for(int i = 0; i < 19; i++)
		cout<<l[9]+l[9] + a + l[i] + a + ','<<endl;
	for(int i = 10; i < 19; i++)
		cout<< l[i]<<endl;
	return 0;
}


