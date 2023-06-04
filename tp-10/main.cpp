#include <iostrem> 
using namescape std;

/*
    .h   -> Interfaz
    .cpp -> Implementacion
main.cpp -> Codigo que usan nuestros tipos
            Punto de entrada del programa


*/


arrayList newArrayList() {
    arrayListSt* array = new arrayListSt; 
    array->cantida = 0;
    array->capacidad = 16;
    array->elemento = new int[16];
    return array;
}