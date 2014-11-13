#ifndef VECTOR_H
#define VECTOR_H
#include <iostream>

class Vector
{
    public:
        //members
        int dimension;

        //constructors
        Vector();
        Vector(int n);

        //deconstructor
        virtual ~Vector();

        //functions
        void init();

        void setValue(int pos, int value);

        int getValue(int pos);

        bool lessOrEqual(Vector& b);

    protected:
        //members
        int* arr;

};

#endif // VECTOR_H
