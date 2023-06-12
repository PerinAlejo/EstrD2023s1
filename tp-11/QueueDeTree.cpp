#include "Tree.h"
#include "QueueDeTree.h"

struct NodoQ {
    Tree elem; // valor del nodo
    NodoQ* siguiente; // puntero al siguiente nodo
};

struct QueueDeTreeSt {
    int cantidad; // cantidad de elementos
    NodoQ* primero; // puntero al primer nodo
    NodoQ* ultimo; // puntero al ultimo nodo
};


//Crea una lista vacía.
//Costo: O(1).
QueueDeTree emptyQ() {
    QueueDeTreeSt* q = new QueueDeTreeSt;
    q->cantidad = 0;
    q->primero = nullptr;
    q->ultimo = nullptr;
    return q;
}

//Indica si la lista está vacía.
//Costo: O(1).
bool isEmptyQ(QueueDeTree q) {
    return (q->cantidad == 0);
}

//Devuelve el primer elemento.
//Prec: No esta vacia.
//Costo: O(1).
Tree firstQ(QueueDeTree q) {
    return q->primero->elem;
}

//Agrega un elemento al final de la cola.
//Costo: O(1).
void Enqueue(Tree t, QueueDeTree q) {
    NodoQ* nodo = new NodoQ;
    nodo->elem = t;
    nodo->siguiente = nullptr;
    if (q->primero == nullptr) {q->primero = nodo;           }
    else                       {q->ultimo->siguiente = nodo; }
    q->ultimo = nodo;
    q->cantidad++;
}

//Quita el primer elemento de la cola.
//Prec: No esta vacia.
//Costo: O(1).
void Dequeue(QueueDeTree q) {
    NodoQ* temp = q->primero;
    q->primero = temp->siguiente;
    delete temp;
    q->cantidad--;
}

//Devuelve la cantidad de elementos de la cola.
//Costo: O(1).
int lengthQ(QueueDeTree q) {
    return q->cantidad;
}

//Anexa q2 al final de q1, liberando la memoria inservible de q2 en el proceso.
//Nota: Si bien se libera memoria de q2, no necesariamente la de sus nodos.
//Costo: O(1).
void MergeQ(QueueDeTree q1, QueueDeTree q2) {
    q1->ultimo->siguiente = q2->primero;
    q1->ultimo = q2->ultimo;
    q1->cantidad += q2->cantidad;
    delete q2;
}

//Libera la memoria ocupada por la lista.
//Costo: O(n).
void DestroyQ(QueueDeTree q) {
    NodoQ* temp = q->primero;
    while(q->primero != nullptr) {
        q->primero = q->primero->siguiente;
        delete temp;
        temp = q->primero;
    }
    delete q;
}