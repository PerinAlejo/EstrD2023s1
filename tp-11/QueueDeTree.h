#include "Tree.h"

struct QueueDeTreeSt;
typedef QueueDeTreeSt* QueueDeTree;


//Crea una lista vacía.
//Costo: O(1).
QueueDeTree emptyQ();

//Indica si la lista está vacía.
//Costo: O(1).
bool isEmptyQ(QueueDeTree q);

//Devuelve el primer elemento.
//Costo: O(1).
Tree firstQ(QueueDeTree q);

//Agrega un elemento al final de la cola.
//Costo: O(1).
void Enqueue(Tree x, QueueDeTree q);

//Quita el primer elemento de la cola.
//Costo: O(1).
void Dequeue(QueueDeTree q);

//Devuelve la cantidad de elementos de la cola.
//Costo: O(1).
int lengthQ(QueueDeTree q);

//Anexa q2 al final de q1, liberando la memoria inservible de q2 en el proceso.
//Nota: Si bien se libera memoria de q2, no necesariamente la de sus nodos.
//Costo: O(1).
void MergeQ(QueueDeTree q1, QueueDeTree q2);

//Libera la memoria ocupada por la lista.
//Costo: O(n).
void DestroyQ(QueueDeTree q);
