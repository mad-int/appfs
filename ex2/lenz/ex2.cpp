#include <iostream>
#include <string>
#include <vector>
#define PREVECTORLENGTH 9
#define VECTORLENGTH 22
using namespace std;
main(void)
{
  vector<string> data;
  data.push_back("#include <iostream>");
  data.push_back("#include <string>");
  data.push_back("#include <vector>");
  data.push_back("#define PREVECTORLENGTH 9");
  data.push_back("#define VECTORLENGTH 22");
  data.push_back("using namespace std;");
  data.push_back("main(void)");
  data.push_back("{");
  data.push_back("  vector<string> data;");
  data.push_back("  for(int i = 0; i < PREVECTORLENGTH; i++)");
  data.push_back("  {");
  data.push_back("    cout << data.at(i) << endl;");
  data.push_back("  }");
  data.push_back("  for(int i = 0; i < VECTORLENGTH; i++)");
  data.push_back("  {");
  data.push_back("    cout << \"  data.push_back(\" << data.at(i) << \")\" << endl;");
  data.push_back("  }");
  data.push_back("  for(int i = PREVECTORLENGTH; i < VECTORLENGTH; i++)");
  data.push_back("  {");
  data.push_back("    cout << data.at(i) << endl;");
  data.push_back("  }");
  data.push_back("}");
  for(int i = 0; i < PREVECTORLENGTH; i++)
  {
    cout << data.at(i) << endl;
  }
  for(int i = 0; i < VECTORLENGTH; i++)
  {
    cout << "  data.push_back(" << data.at(i) << ")" << endl;
  }
  for(int i = PREVECTORLENGTH; i < VECTORLENGTH; i++)
  {
    cout << data.at(i) << endl;
  }
}