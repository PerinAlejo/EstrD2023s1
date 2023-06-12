#include "LinkedList.h"
#include "Set.h"
#include "Tree.h"
#include "QueueDeTree.h"
#include "ArrayList.h"
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

int unoSi(bool expr) {
    return (expr) ? 1 : 0;
}

//Dado un árbol binario de enteros devuelve la suma entre sus elementos.
int sumarT(Tree t){
    if (isEmptyT(t)) {
        return 0;
    } else {
        return rootT(t) + sumarT(left(t)) + sumarT(right(t));
    }
}

//Iterativo
int sumarTI(Tree t) {
    QueueDeTree q =  emptyQ();
    int sumatoria = 0;
    if  (!isEmptyT(t))  {
        Enqueue(t,q);
    }
    while (!isEmptyQ(q)) {
        Tree actual = firstQ(q);
        Dequeue(q);
        sumatoria += rootT(actual); // += es lo mismo que ap = ap + unoSi
        if(!isEmptyT(left(actual))) {
            Enqueue(left(actual), q);
        }
        if(!isEmptyT(right(actual))) {
            Enqueue(right(actual), q);
        }
    }
    DestroyQ(q);
    return sumatoria;
}

//Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
//en inglés)
int sizeT(Tree t)  {
    if (isEmptyT(t)) {
        return 0;
    } else {
        return 1 + sizeT(left(t)) + sizeT(right(t));
    }
}

//Iterativo
int sizeTI(Tree t) {
    QueueDeTree q =  emptyQ();
    int size = 0;
    if  (!isEmptyT(t))  {
        Enqueue(t,q);
    }
    while (!isEmptyQ(q)) {
        Tree actual = firstQ(q);
        Dequeue(q);
        size += 1; // += es lo mismo que ap = ap + unoSi
        if(!isEmptyT(left(actual))) {
            Enqueue(left(actual), q);
        }
        if(!isEmptyT(right(actual))) {
            Enqueue(right(actual), q);
        }
    }
    DestroyQ(q);
    return size;
}

//Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
//árbol.
bool perteneceT(int e, Tree t) {
    if (isEmptyT(t)) {
        return false;
    } else {
        return (e == rootT(t)) || perteneceT(e, left(t)) || perteneceT(e, right(t));
    }
}

//Iterativo
int perteneceTI(int e, Tree t) {
    QueueDeTree q =  emptyQ();
    int pertenece = false;
    if  (!isEmptyT(t))  {
        Enqueue(t,q);
    }
    while (!isEmptyQ(q) && !pertenece) {
        Tree actual = firstQ(q);
        Dequeue(q);
        pertenece = rootT(actual) == e; // += es lo mismo que ap = ap + unoSi
        if(!isEmptyT(left(actual))) {
            Enqueue(left(actual), q);
        }
        if(!isEmptyT(right(actual))) {
            Enqueue(right(actual), q);
        }
    }
    DestroyQ(q);
    return pertenece;
}

//Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
//iguales a e
int aparicionesT(int e, Tree t) {
    if(isEmptyT(t)) {
        return 0;
    } else {
        int unoSiRaiz = unoSi(e == rootT(t));
        return unoSiRaiz + aparicionesT(e, left(t)) + aparicionesT(e, right(t));
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
            Enqueue(left(actual), q);
        }
        if(!isEmptyT(right(actual))) {
            Enqueue(right(actual), q);
        }
    }
    DestroyQ(q);
    return apariciones;
}

int maximo(int x, int y) {
    return (y > x) ? y : x;
}

//Dado un árbol devuelve su altura.
int heightT(Tree t) {
    if(isEmptyT(t)) {
        return 0;
    } else {
        return 1 + maximo(heightT(left(t)), heightT(right(t)));
    }
}

ArrayList append(ArrayList xs, ArrayList ys){
    if (lengthAL(xs) + lengthAL(ys) > capacidad(xs))  {
        resize(lengthAL(xs) + lengthAL(ys), xs);
    }

    for (int i = 1; i <= lengthAL(ys); i++ ){
        add(get(i,ys), xs);
    }
    return xs; 
}

//Dado un árbol devuelve una lista con todos sus elementos.
ArrayList toList(Tree t) {
    if(isEmptyT(t)) {
        return newArrayList();
    } else {
        ArrayList root = newArrayListWith(1);
        add(rootT(t), root);
        return append ((append(root ,toList(left(t)))), toList(right(t)));
    }
}

ArrayList toListI(Tree t) {
    QueueDeTree q = emptyQ();
    ArrayList ts = newArrayList();
    if (!isEmptyT(t)) {
        Enqueue(t,q);
    }
    while (!isEmptyQ(q)) {
        Tree actual = firstQ(q);
        Dequeue(q);
        add(rootT(actual), ts);
        if(!isEmptyT(left(actual))) {
            Enqueue(left(actual), q);
        }
        if(!isEmptyT(right(actual))) {
            Enqueue(right(actual), q);
        }
    }
    DestroyQ(q);
    return ts;
}

//Dado un árbol devuelve los elementos que se encuentran en sus hojas.
ArrayList leaves(Tree t) {
    if(isEmptyT(t)) {
        return newArrayList();
    } else {
        if (isEmptyT(left(t)) && isEmptyT(right(t))) {
            ArrayList ts = newArrayListWith(1);
            add(rootT(t),ts);
            return ts;
        } else {
            return append(leaves(left(t)), leaves(right(t)));
        }
    }
}

//Dados un número n y un árbol devuelve una lista con los nodos de nivel n
ArrayList levelN(int n, Tree t) {
    ArrayList ts = newArrayList();
    if(isEmptyT(t)) {
        return ts;
    } else if (n == 0) {
         add(rootT(t), ts);
         return ts;
    } else {
        return append(levelN((n-1),left(t)),levelN((n-1), right(t)));
    }
    
}

// ====================================================================================================
// ===========================================main=====================================================
// ====================================================================================================

void ShowArrayList(ArrayList xs) {
    cout <<"[";
    cout << get(1, xs);
    for (int i = 2; i <= lengthAL(xs); i++ ){
        cout << ", " << get(i, xs);
    }
    cout << "]" << endl; 
}


int main() {
    Tree t = 
    nodeT(1,
        nodeT(2,(nodeT (4, emptyT(), emptyT())),(nodeT (5, emptyT(),emptyT()))), 
        nodeT(3,(nodeT (6, emptyT(), emptyT())),(nodeT (7, emptyT(),emptyT()))));
    ShowArrayList(toListI(t));
    
}

