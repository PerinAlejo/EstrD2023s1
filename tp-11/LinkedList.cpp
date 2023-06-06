#include "LinkedList.cpp"

struct NodoL {
    int elem; // valor del nodo
    NodoL* siguiente; // puntero al siguiente nodo
};

struct LinkedListSt {
// INV.REP.: cantidad indica la cantidad de nodos que se pueden recorrer
// desde primero por siguiente hasta alcanzar a NULL
    int cantidad; // cantidad de elementos
    NodoL* primero; // puntero al primer nodo
};

struct IteratorSt {
    NodoL* current;
};

//Crea una lista vacía.
//Eficiencia: Constante ya que crear y almacenar es O(1)
LinkedList nil() {
    LinkedListSt* xs = new LinkedListSt;
    xs->cantidad = 0;
    xs->primero = NULL;
    return xs;
}

//Indica si la lista está vacía.
//Eficiencia: O(1) ya que se hace una comparacion.
bool isEmpty(LinkedList xs){
    return (xs->cantidad == 0);
}

//Devuelve el primer elemento.
//Prec: La lista no es vacia.
//Eficiencia: O(1)
int head(LinkedList xs) {
    return xs->primero->elem;
}

//Agrega un elemento al principio de la lista.
//Eficiencia: O(1)
void Cons(int x, LinkedList xs){
    NodoL* n = new NodoL;
    n->elem = x; 
    n->siguiente = xs->primero;
    xs->primero = n;
    xs->cantidad++;
}

//Quita el primer elemento.
//Prec: La lista no es vacia.
void Tail(LinkedList xs) {
    NodeL* temp = xs->primero; 
    xs->primero = xs->primero->siguiente
    delete temp;
    xs->cantidad--;
}

//Devuelve la cantidad de elementos.
int length(LinkedList xs) {
    return xs->cantidad;
}

//Agrega un elemento al final de la lista.
//Eficiencia: O(n) donde n son los elementos de la lista
void Snoc(int x, LinkedList xs) {
    NodeL* node = new NodeL;
    node->elem = x;
    node->siguiente = NULL;
    if (xs->primero == NULL) {
        xs->primero = node;
    } else {
        NodeL* temp = xs->primero;
        while (temp->siguiente != NULL) {
            temp = temp->siguiente;
        }
        temp->siguiente = node;
        delete temp;
    }
    xs->cantidad++;
}

//Apunta el recorrido al primer elemento.
ListIterator getIterator(LinkedList xs) {
    IteratorSt* ixs = new IteratorSt;
    ixs->current = xs->primero;
    return ixs;
}

//Devuelve el elemento actual en el recorrido.
int current(ListIterator ixs) {
    return ixs->current->elem;
}

//Reemplaza el elemento actual por otro elemento.
void SetCurrent(int x, ListIterator ixs) {
    ixs->current->elem = x;
}

//Pasa al siguiente elemento.
void Next(ListIterator ixs){
    ixs->current = ixs->current->siguiente;
}

//Indica si el recorrido ha terminado.
bool atEnd(ListIterator ixs) {
    return (ixs->current->siguiente == NULL)
}

//Libera la memoria ocupada por el iterador.
void DisposeIterator(ListIterator ixs){
    delete ixs;
}

//Libera la memoria ocupada por la lista. Crea y borra espacios de memoria tantas veces como elementos 
//a borrar.
void DestroyL(LinkedList xs) {    
    if(xs->primero == NULL) {
        delete xs;
    } else {
        NodeT* tempSiguiente = xs->primero;
        while (tempSiguiente->siguiente != NULL) {
            NodeT* tempABorrar = tempSiguiente;
            tempSiguiente = tempABorrar->siguiente;
            delete tempABorrar;
        }
        delete tempSiguiente;
        delete xs;
    }
}

//Libera la memoria ocupada por la lista. Utiliza Iterador
void DestroyLConIterador(LinkedList xs) {
    if(xs->primero == NULL) {
        delete xs;
    } else {
        ListIterator tempABorrar = getIterator(xs);
        ListIterator tempSiguiente;
        while (not atEnd(tempABorrar)) {
            tempSiguiente = next(getIterator(xs));
            delete tempABorrar->current;
            tempABorrar = tempSiguiente;
        }
        delete tempABorrar->current;
        DisposeIterator();
        DisposeIterator();
        delete xs;
    }   
}


