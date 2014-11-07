#include <iostream>
#include <fstream>
#include <algorithm>
#include <vector>


using namespace std;


int main(){

vector<int> foo;
foo.reserve(500000002);

ifstream file("ndata.dat",ios::binary);

int k;
while(!file.eof()){
    file.read((char*)&k, 4);
    foo.push_back(k);
}
sort(foo.begin(), foo.end());
cout << foo.size() << endl;
cout << foo[0] << endl;

//----Ausgabe----

for(int j = 1; j < foo.size(); j++){
    if(!(foo[j]==foo[j-1])&&(foo[j] >= 0))
        cout << foo[j] << endl;
}

file.close();
return 0;
}
