/*
 * main.cpp
 *
 *  Created on: Oct 20, 2014
 *      Author: andreas
 */

#include <iostream>
#include <fstream>
using namespace std;


void mem(int** p,int n)
{
	try
		{
			*p=new int[n];
		}
		catch(bad_alloc &e)
		{
			cout<<"no memory available";
		}
}

void QuickSort (int* a, int left, int right)
{
	if (left < right)
	{
		int pivot = a[right], l = left, r = right;
		do
		{
			while (a[l] < pivot) l++;
			while (a[r] > pivot) r--;
			if (l <= r)
			{
				int tmp = a[l];
				a[l] = a[r];
				a[r] = tmp;
				l++;
				r--;
			}
		}
		while (l <= r);
		QuickSort (a, left, r);
		QuickSort (a, l, right);
	}
}


int main(int argc, char* argv[] )
{
	if(argc !=2)
	{
		cout<<"you need to type in the data-file to be read"<<endl;
		return 0;
	}
	int *p=NULL;
	mem(&p,500000020);									//memory allocation

	ifstream datei(argv[1],ios::binary | ios::in);		//argv[1] is binary file to be read
	if (datei.fail()==true)
	{
		cout<<"failed to open ndata.dat"<<endl;
		return 0;
	}
	int n=0;

	//reading the integers into the array
	while(datei.operator!()!=true)
	{
		datei.read((char*)&p[n],4);						//reading 4 byte units and writing them to the array
		++n;
	}
	datei.close();

	//sorting the array
	QuickSort(p,0,n-2);

	//writing the non-negative numbers sorted into a .txt file
	ofstream sorted("sorted-numbers.txt");
	for(int i=0; i<n-1; ++i)
	{
		if(p[i]>=0)
		{
			sorted << p[i]<<"\t \t"<<(char)p[i]<<endl;
		}
	}
	delete [] p;
	sorted.close();
	return 0;
}



