#include "ArrayList.h"

struct ArrayListSt {
    int cantidad; // cantidad de elementos
    int* elementos; // array de elementos
    int capacidad; // tamaño del array
};

ArrayList newArrayList() {
    ArrayListSt* array = new ArrayListSt; 
    array->cantidad = 0;
    array->capacidad = 16;
    array->elementos = new int[16];
    return array;
}

ArrayList newArrayListWith(int capacidad) {
    ArrayListSt* array = new ArrayListSt; 
    array->cantidad = 0;
    array->capacidad = capacidad;
    array->elementos = new int[capacidad];
    return array;
}

int lengthAL(ArrayList xs) {
    return xs->cantidad;
}

int get(int i, ArrayList xs) {
    return xs->elementos[i-1];
}

//PREC: i debe ser mayor a 0
void set(int i, int x, ArrayList xs) {
    xs->elementos[i-1] = x;
}

void resize(int capacidad, ArrayList xs) {
    int* temp = new int[capacidad];
    for (int i= 0; i < capacidad ; i++) {
        temp[i] = xs->elementos[i];
    }
    delete xs->elementos;
    xs->elementos = temp;
    xs->capacidad = capacidad;
    if (xs->cantidad > xs->capacidad)  {xs->cantidad = xs->capacidad;}
}

void add(int x, ArrayList xs){
    if (xs->cantidad == xs->capacidad) {
        resize(xs->cantidad * 2, xs);
    }
    xs->elementos[xs->cantidad] = x;
    xs->cantidad++;
}

int capacidad(ArrayList xs) {
    return xs->capacidad;
}

void remove(ArrayList xs) {
    delete xs->elementos;
    delete xs;
}






