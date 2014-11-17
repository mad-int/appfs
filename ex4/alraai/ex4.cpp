#include <iostream>
#include <stdio.h>   // fopen
#include <string.h>  // strpbrk
#include <assert.h>  // assert
#include <math.h>

//Das mit dem Einlesen hat nicht funktioniert, dafür reichen leider meine cpp Kenntnisse nicht..

using namespace std;

struct Vector{
    int dimension;
    int* array;

    Vector(int n){
        assert(n>0);
        dimension = n;
        array = new int[n];
        for(int i = 0; i < n; i++)
            array[i] = 0;
    }
    void setValue(int pos, int value){
        assert(0 <=pos && pos < dimension);
        array[pos] = value;
    }
    int getValue(int pos){
        assert(pos>=0&&pos<dimension);
        return array[pos];
    }

    bool lessOrEqual(Vector& b){
    assert(dimension>0 && dimension == b.dimension);
    for(int i = 0; i < dimension; i++)
        if(array[i] > b.getValue(i)) return false;
    return true;
}
};

struct Matrix{
    int rows;
    int columns;
    int** array2D;

    Matrix(int m, int n){
        rows    = m;
        columns = n;
        array2D = new int* [rows];

        for(int i=0; i<rows; i++)
            array2D[i] = new int[columns];
        for(int j=0; j<rows; j++){
            for(int k=0; k<columns; k++)
                array2D[j][k] = 0;
        }
    }

    void setValue(int m, int n, int value){
        array2D[m][n] = value;
    }
    int getValue(int m, int n){
        return array2D[m][n];
    }

    Vector multiply(Vector& x){
    Vector sum(rows);
    for(int i = 0; i<rows;i++){
        for(int j = 0; j<columns;j++){
            sum.setValue(i,sum.getValue(i)+x.getValue(j)*array2D[i][j]);
         }
    }
    return sum;
}

};

void dec2bin(int decimal, Vector& binary){
    assert(decimal>=0);
    int counter = binary.dimension-1;
    while(decimal){
        binary.setValue(counter,decimal % 2);
        decimal = decimal / 2;
        counter--;
    }
}


int main()
{

    int m;
    int n;
    int l = 0;

    cout << "Bitte geben Sie die Zeilenanzahl an: " << endl;
    cin >> m;
    cout << "Bitte geben Sie die Spaltenanzahl an: " << endl;
    cin >> n;


    Matrix matrix(m,n);
    Vector b(m);

    cout << "Nun füllen Sie ihre Matrix von links oben nach rechts oben und Entern Sie mit jeder Zahl: " << endl;
    l= 0;

    for(int i = 0; i < m; i++){
        for(int j = 0; j < n; j++){
            int a;
            cin >> a;
            if(l < 32){
                matrix.setValue(i,j,a);
                l++;
            }else{
                break;
            }
        }
    }
    l = 0;
    cout << "Nun geben Sie ihren Vektor an: " << endl;

    for(int k = 0; k < m; k++){
        int c;
        cin >> c;
        if(l < m){
            b.setValue(k,c);
            l++;
        }else{
            break;
        }
    }

    cout << "Gesuchte Lösung: " << endl;

    int maxSolutions = pow(2,n)-1;
    for(int k = 0;k <= maxSolutions;k++){
        Vector x(n);
        dec2bin(k,x);
        Vector sum = matrix.multiply(x);
        if(sum.lessOrEqual(b)){
            cout << "x_" << k << " = " <<"(" <<  x.getValue(0) << "," << x.getValue(1) << "," << x.getValue(2) << ")\n"<< endl;
        }
    }
    return 0;
}
