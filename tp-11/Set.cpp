#include "Set.h"
#include "LinkedList.h"

struct NodoS {
    int elem; // valor del nodo
    NodoS* siguiente; // puntero al siguiente nodo
};

struct SetSt {
    //INV.REP: El set no debe tener elementos repetidos.
    int cantidad; // cantidad de elementos diferentes
    NodoS* primero; // puntero al primer nodo
};

//Crea un conjunto vacío.
//Eficiencia: O(1)
Set emptyS() {
    SetSt* set = new SetSt;
    set->cantidad = 0;
    set->primero = nullptr;
    return set;
}

//Indica si el conjunto está vacío.
//Eficiencia: O(1)
bool isEmptyS(Set s) {
    return (s->cantidad == 0);
}

//Indica si el elemento pertenece al conjunto.
//Eficiencia: O(n) donde n son los elementos del Set
bool belongsS(int x, Set s) {
    NodoS* temp = s->primero;
    bool pertenece = false;
    while(temp != nullptr && !pertenece) {
        pertenece = temp->elem == x;
        temp = temp->siguiente;
    }
    return pertenece;
}

//Agrega un elemento al conjunto.
void AddS(int x, Set s) {
    if(!belongsS(x, s)) {
        NodoS* nodo = new NodoS;
        nodo->elem = x;
        nodo->siguiente = s->primero;
        s->primero = nodo;
        s->cantidad++;
    }
}

//Quita un elemento dado.
//Eficiencia: O(n) donde n son los elementos del Set
void RemoveS(int x, Set s) {
    NodoS* temp = s->primero;
    NodoS* tempAnterior;
    while (temp != nullptr && temp->elem != x) {
        tempAnterior = temp;
        temp = temp->siguiente;
        
    }
    if (temp != nullptr) {
        tempAnterior->siguiente = temp->siguiente;
        delete temp;
    }
    s->cantidad--;
}

//Devuelve la cantidad de elementos.
//Eficiencia: O(1)
int sizeS(Set s) {
    return s->cantidad;
}

//Devuelve una lista con los lementos del conjunto.
LinkedList setToList(Set s){
    LinkedList l = nil();
    NodoS* temp = s->primero;
    while(temp != nullptr) {
        Cons(temp->elem, l);
        temp = temp->siguiente;
    }
    return l;
}

//Libera la memoria ocupada por el conjunto.
void DestroyS(Set s){
    NodoS* temp = s->primero;
    while (s->primero != nullptr) {
        s->primero = s->primero->siguiente;
        delete temp;
        temp = s->primero;
    }
    delete s;
}



