#include "Matrix.h"

Matrix::Matrix(){
}

Matrix::Matrix(int m, int n)
{
    assert(0<m&&0<n);
    rows    = m;
    columns = n;
    arr.reserve(rows*columns);
    for(int i=0; i<rows*columns; i++){
            arr.push_back(0);
    }
}

Matrix::~Matrix(){
}

void Matrix::init(){
    assert(0<rows&&0<columns);
    arr.reserve(rows*columns);
    for(int j=0; j<rows; j++){
        for(int k=0; k<columns; k++)
            arr[j*columns+k] = 0;
    }
}

void Matrix::setValue(int m, int n, int value){
    assert(0 <= m && m < rows && 0 <= n && n < columns);
    arr[m*columns+n] = value;
}

int Matrix::getValue(int m, int n){
    assert(0 <= m && rows > m && 0 <= n && columns > n);
    return arr[m*columns+n];
}

Vector Matrix::multiply(Vector& x){
    assert(0 < x.dimension && x.dimension == columns);
    Vector sum(rows);
    for(int i = 0; i<rows;i++){
        for(int j = 0; j<columns;j++){
            sum.setValue(i,sum.getValue(i)+x.getValue(j)*arr[i*columns+j]);
        }
    }
    return sum;
}

