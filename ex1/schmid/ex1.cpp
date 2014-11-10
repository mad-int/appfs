#include <iostream>
#include <stdio.h>
#include <algorithm>


int main()
{

  FILE *Quelldatei;

  Quelldatei = fopen("ndata.dat", "r");

  int* buffer = new int[500000001];
  fread((void*)buffer,sizeof(int),500000001,Quelldatei);
  
  std::sort(buffer, buffer + 500000001);
  
  int i = 0;
  for (i;i<500000001;++i){
    if (buffer[i]>=0){
      break;
    }    
  }
  
  std::cout << buffer[i] << std::endl;
  int x_tmp = buffer[i];
  for (int j = i-1; j<500000001;++j){
    if (buffer[j]>x_tmp){
      std::cout << buffer[j] << std::endl;
      x_tmp = buffer[j];
    }  
  }
  fclose(Quelldatei);
  delete[] buffer;
}


