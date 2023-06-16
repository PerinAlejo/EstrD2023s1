#include <iostream>
#include <iomanip>
#include <algorithm>
#include "TiposBasicos.h"
#include "TableroInfinito.h"
#include "BiBST.h"
using namespace std;

//==========================================================================
// Implementación de TableroInfinito
//==========================================================================
struct TableroInfinitoHeader {
  BiBST tablero; //Tablero
  int ax;         //Coordenada x de la celda actual
  int ay;         //Coordenada y de la celda actual
}; 
/* INV.REP.:
    - ax representa el kx del nodo con el que se quiere interactuar.(Siendo kx la primera clave del nodo del tablero)
    - ay representa el ky del nodo con el que se quiere interactuar.(Siendo ky la segunda clave del nodo del tablero)
    - El tablero debe iniciarse en la celda (0,0).
*/

//--------------------------------------------------------------------------
//PROP: Retorna un tablero infinito vacío (todas las celdas sin bolitas), con la celda actual en
//la posición (0,0)
TableroInfinito TInfInicial(){
  TableroInfinito t = new TableroInfinitoHeader;
  t->tablero = insertBBNode(EMPTYBB, 0, 0);
  t->ax = 0;
  t->ay = 0;
  return t; // REEMPLAZAR
}

//--------------------------------------------------------------------------
//PROP: Modifica el tablero dado para agregar n bolitas del color dado a la celda actual del tablero
// PRECOND: el color es válido
void PonerNTInf(TableroInfinito t, Color color, int n){
    if (n >=0) {
      BiBST celdaActual = insertBBNode(t->tablero, t->ax, t->ay);
      celdaActual->bolitas[color] += n;
    } else {
      BOOM("No se puede poner una cantidad negativa de bolitas");
    }    
}

//--------------------------------------------------------------------------
// PROP: Modifica el tablero dado para sacar n bolitas del color dado de la celda 
// actual del tablero, si existe esa cantidad, o falla con BOOM en otro caso,
// PRECOND:
//   el color es válido
//   hay al menos n bolitas en la celda actual en t
void SacarNTInf(TableroInfinito t, Color color, int n){
  BiBST celdaActual = findBBNode(t->tablero, t->ax, t->ay);
  if (celdaActual != NULL && celdaActual->bolitas[color] >= n ) {
    celdaActual->bolitas[color] -= n;
  } else {
    BOOM("No hay suficientes bolitas");
  }
}

//--------------------------------------------------------------------------
// PROP: Modifica el tablero dado para mover n posiciones en la dirección dada a la celda actual
// PRECOND: la dirección dada es válida
void MoverNTInf(TableroInfinito t, Dir dir, int n){
  switch (dir)
  {
  case NORTE:
    t->ay += n; 
    break;
  case SUR:
    t->ay -= n;
    break;
  case ESTE:
    t->ax += n;
    break;
  case OESTE:
    t->ax -= n;
    break;
  default:
    cerr << "ERROR: " << dir << " no es una representación de una dirección válida";
    break;
  }
}

//--------------------------------------------------------------------------
// PROP:retorna el número de bolitas de ese color en la celda actual del tablero dado
// PRECOND: el color es válido
int nroBolitasTInf(TableroInfinito t, Color color) {
  BiBST celdaActual = findBBNode(t->tablero, t->ax, t->ay);
  if (celdaActual == NULL) {
    return 0;
  } else {
    return celdaActual->bolitas[color];
  }
  
}

//--------------------------------------------------------------------------
// PROP: Libera toda la memoria del tablero dado.
void LiberarTInf(TableroInfinito t){
  LiberarBiBST(t->tablero);
  delete t;
}

//==========================================================================
// Impresión para verificaciones
//==========================================================================
void PrintRepTInf(TableroInfinito t) {
  cout << "Celda actual: (" << t->ax << ", " << t->ay << ")" << endl;  
  PrintBB(t->tablero);
  cout << endl;
}
