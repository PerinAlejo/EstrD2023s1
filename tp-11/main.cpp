#include "LinkedList.h"
#include "Set.h"
#include <iostream>
using namespace std;

// ====================================================================================================
// ======================================Ejercicio 2===================================================
// ====================================================================================================

//Devuelve la suma de todos los elementos.
int sumatoria(LinkedList xs) {
    ListIterator ixs = getIterator(xs);
    int sum = 0;
    while(!atEnd (ixs)) {
        sum += current(ixs);
        Next(ixs);
    }
    DisposeIterator(ixs);
    return sum;
}

//Incrementa en uno todos los elementos.
void Sucesores(LinkedList xs) {
    ListIterator ixs = getIterator(xs);
    while(!atEnd (ixs)) {
        SetCurrent(current(ixs)+1, ixs);
        Next(ixs);
    }
    DisposeIterator(ixs);
}

//Indica si el elemento pertenece a la lista.
bool pertenece(int x, LinkedList xs) {
    ListIterator ixs = getIterator(xs);
    bool pertenece = false;
    while(!atEnd (ixs) && !pertenece) {
        pertenece = current(ixs) == x;
        Next(ixs);
    } 
    DisposeIterator(ixs);
    return pertenece;
}

//Indica la cantidad de elementos iguales a x
int apariciones(int x, LinkedList xs) {
    ListIterator ixs = getIterator(xs);
    int apariciones = 0;
    while(!atEnd (ixs)) {
        apariciones += (current(ixs) == x) ? 1 : 0;
        Next(ixs);
    }
    DisposeIterator(ixs);
    return apariciones;
}

int minimo(LinkedList xs) {
    ListIterator ixs = getIterator(xs);
    int min = current(ixs);
    Next(ixs); 
    while (!atEnd(ixs)) {
        min = ( current(ixs) < min) ? current(ixs) : min;
        Next(ixs); 
    }
    DisposeIterator(ixs);
    return min;
}

LinkedList copy(LinkedList xs) {
    LinkedList newList = nil();
    ListIterator ixs = getIterator(xs);
    while(!atEnd(ixs)) {
        Snoc(current(ixs), newList);
        Next(ixs);
    }
    DisposeIterator(ixs);
    return newList;
}

void ShowList(LinkedList xs) {
    ListIterator ixs = getIterator(xs);
    cout << "[" << current(ixs);
    Next(ixs);
    while(!atEnd(ixs)) {
        cout <<", "<< current(ixs);
        Next(ixs);
    }
    cout << "]" << endl;
    DisposeIterator(ixs);
}


// ====================================================================================================
// ======================================Ejercicio 7 y 8===============================================
// ====================================================================================================
/*
int unoSi(bool expr) {
    return (expr) ? 1 : 0;
}

//Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
//iguales a e
int aparicionesT(int e, Tree t) {
    if(isEmptyT) {
        return 0;
    } else {
        int unoSiRaiz = unoSi(e == rootT(t));
        return valorRaiz + aparicionesT(e, left(t)) + aparicionesT(e, right(t));
    }
}

//iterativo
int aparicionesTI(int e, Tree t) {
    QueueDeTree q = emptyQ();
    int apariciones = 0;
    if (!isEmptyT(t)) {
        Enqueue(t,q);
    }
    while (!isEmptyQ(q)) {
        Tree actual = firstQ(q);
        Dequeue(q);
        apariciones += unoSi(rootT(actual) == e); // += es lo mismo que ap = ap + unoSi
        if(!isEmptyT(left(actual))) {
            Enqueue(left(actual));
        }
        if(!isEmptyT(right(actual))) {
            Enqueue(right(actual));
        }
    }
    DestroyQ(q);
    return apariciones;
}
*/

// ====================================================================================================
// ===========================================main=====================================================
// ====================================================================================================

int main() {
    Set s = emptyS();
    cout << "Esta vacio:" << isEmptyS(s) << endl;
    AddS(1, s);
    AddS(2, s);
    AddS(3, s);
    AddS(3, s);
    LinkedList l = setToList(s);
    ShowList(l);
    cout << "Pertenece el 5: " << belongsS(5, s) << endl;
    RemoveS(2, s);
    cout << "El tamanio es: " << sizeS(s) << endl;
    l = setToList(s);
    ShowList(l);
    DestroyS(s);
}

