#include "Matrix.h"

Matrix::Matrix(){}

Matrix::Matrix(int m, int n)
{
    rows    = m;
    columns = n;
    arr = new int(rows*columns);
    for(int j=0; j<rows; j++){
        for(int k=0; k<columns; k++)
            arr[j*columns+k] = 0;
    }
}

Matrix::~Matrix(){
    delete arr;
}


void Matrix::setValue(int m, int n, int value){
    arr[m*columns+n] = value;
}

int Matrix::getValue(int m, int n){
    return arr[m*columns+n];
}

Vector Matrix::multiply(Vector& x){
    Vector sum(rows);
    for(int i = 0; i<rows;i++){
        for(int j = 0; j<columns;j++){
            sum.setValue(i,sum.getValue(i)+x.getValue(j)*arr[i*columns+j]);
        }
    }
    return sum;
}

