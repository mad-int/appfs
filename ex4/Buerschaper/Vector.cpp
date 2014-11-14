#include "Vector.h"

Vector::Vector(){}

Vector::Vector(int n){
    dimension = n;
    arr.reserve(dimension);
    for(int i = 0; i < dimension; i++)
         arr.push_back(0);
}

Vector::~Vector(){
}

void Vector::init(){
    arr.reserve(dimension);
    for(int i = 0; i < dimension; i++)
         arr.push_back(0);
}

void Vector::setValue(int pos, int value){
    assert(0 <= pos && pos < dimension);
    arr[pos] = value;
}

int Vector::getValue(int pos){
    assert(0 <= pos && pos < dimension);
    return arr[pos];
}

bool Vector::lessOrEqual(Vector& b){
    for(int i = 0; i < dimension; i++){
        if(arr[i] > b.getValue(i)) return false;
    }
    return true;
}
