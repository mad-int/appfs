#include "Vector.h"

Vector::Vector(){}

Vector::Vector(int n){
    dimension = n;
    arr = new int(dimension);
    for(int i = 0; i < n; i++)
        arr[i] = 0;
}

Vector::~Vector(){
    delete arr;
}

void Vector::init(){
    arr[dimension];
    for(int i = 0; i < dimension; i++)
        arr[i] = 0;
}

void Vector::setValue(int pos, int value){
    arr[pos] = value;
}

int Vector::getValue(int pos){
    return arr[pos];
}

bool Vector::lessOrEqual(Vector& b){
    for(int i = 0; i < dimension; i++){
        if(arr[i] > b.getValue(i)) return false;
    }
    return true;
}
