#include "Persona.h"
#include "Pokemon.h"
#include "Entrenador.h"
#include "ArrayList.h"
#include <iostream>
using namespace std;
/*
    .h   -> Interfaz
    .cpp -> Implementacion
main.cpp -> Codigo que usan nuestros tipos
            Punto de entrada del programa


*/

//Devuelve la suma de todos los elementos.
int sumatoria(ArrayList xs) {
    int sumatoria = 0;
    for (int i = 1; i <= lengthAL(xs); i++) {
        sumatoria = sumatoria + get(i, xs); 
    }
    return sumatoria;
}

//Incrementa en uno todos los elementos.
void sucesores(ArrayList xs) {
    for (int  i = 1; i <= lengthAL(xs); i++ ){
        set(i, get(i,xs) + 1 ,xs);
    }
}

//Indica si el elemento pertenece a la lista.
bool pertenece(int x, ArrayList xs) {
    int i = 1;
    bool pertenece = false;
    while (not pertenece && i <= lengthAL(xs)) {
        pertenece = get(i, xs) == x;
        i++;
    }
    return pertenece;
}

//Indica la cantidad de elementos iguales a x.
int apariciones(int x, ArrayList xs) {
    int count = 0;
    for (int  i = 1; i <= lengthAL(xs); i++ ){
        count = count + (get(i, xs) == x) ? 1 :  0;
    }
    return count;
}

//Crea una nueva lista a partir de la primera y la segunda (en ese orden).
ArrayList append(ArrayList xs, ArrayList ys){
    if (lengthAL(xs) + lengthAL(ys) > capacidad(xs))  {
        resize(lengthAL(xs) + lengthAL(ys), xs);
    }

    for (int i = 1; i <= lengthAL(ys); i++ ){
        add(get(i,ys), xs);
    }
    return xs; 
}

void ShowArrayList(ArrayList xs) {
    cout <<"[";
    cout << get(1, xs);
    for (int i = 2; i <= lengthAL(xs); i++ ){
        cout << ", " << get(i, xs);
    }
    cout << "]" << endl; 
}

int main() {
    ArrayList xs = newArrayList();
    add(1, xs);
    add(2, xs);
    add(3, xs);
}


