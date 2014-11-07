//
//  main.cpp
//  ex1
//
//  Created by Claus Lang on 23/10/14.
//  Copyright (c) 2014 TU. All rights reserved.
//

#include <iostream>
#include <fstream>
#include <list>

using namespace std;

int main(int argc, const char * argv[])
{

    ifstream inFile;
    inFile.open(argv[1], ios::in | ios::binary);
    
    if (!inFile.is_open()) {
        return 1;
    }
    
    inFile.seekg(0,ios::end);
    int filesize = inFile.tellg()/4;
    // only for testing:
    filesize = 1000000;
    ///////
    inFile.seekg(0, ios::beg);
    
    int num;
    int counter = 1;
    int maxvalue = 0;
    list<int> nums;
    list<int>::iterator iter;
    
    for (int i = 0; i<filesize; i++) {
        if (i/1000 == counter) {
            printf("%d 000\n",counter);
            counter++;
        }
        inFile.read((char*)&num,4);
        if(num < 0){
            continue;
        }
        // search:
        if(num > maxvalue) {
            maxvalue = num;
            iter = nums.end();
        } else {
            iter = nums.begin();
            while (num > *iter) {
                iter++;
            }
        }
        nums.insert(iter,num);
    }
    
    list<int>::iterator end = nums.end();
    for(iter = nums.begin();iter != end;++iter){
        printf("%d\n",*iter);
    }

}









