#include "ArrayLists.h"

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

void set(int i, int x, ArrayList xs) {
    xs->elementos[i] = x;
}

void resize(int capacidad, ArrayList xs) {
    
}




