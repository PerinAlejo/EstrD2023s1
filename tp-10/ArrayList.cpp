#include "ArrayLists.h"

struct ArrayListSt {
    int cantidad; // cantidad de elementos
    int* elementos; // array de elementos
    int capacidad; // tamaño del array
}

ArrayList newArrayList() {
    ArrayListSt* array = new arrayListSt; 
    array->cantida = 0;
    array->capacidad = 16;
    array->elemento = new int[16];
    return array;
}

ArrayList newArrayListWith(int capacidad) {
    ArrayListSt* array = new arrayListSt; 
    array->cantida = 0;
    array->capacidad = capacidad;
    array->elemento = new int[capacidad];
    return array;
}

int lengthAL(ArrayList xs) {
    return xs->cantidad;
}

int get(int i, ArrayList xs) {
    return xs->elemento[i-1];
}

void set(int i, int x, ArrayList xs) {
    xs->elemento[i] = x;
}

void resize(int capacidad, ArrayList xs) {
    
}




