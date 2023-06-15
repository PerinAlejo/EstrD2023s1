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
  BiBST tablero;
  BiBST celdaActual;
  // COMPLETAR
}; 
/* INV.REP.:
  * COMPLETAR
*/

//--------------------------------------------------------------------------
//PROP: Retorna un tablero infinito vacío (todas las celdas sin bolitas), con la celda actual en
//la posición (0,0)
TableroInfinito TInfInicial(){
  TableroInfinito t = new TableroInfinitoHeader;
  t->tablero = insertBBNode(EMPTYBB, 0, 0);
  t->celdaActual = t->tablero;
  return t; // REEMPLAZAR
}

//--------------------------------------------------------------------------
//PROP: Modifica el tablero dado para agregar n bolitas del color dado a la celda actual del tablero
// PRECOND: el color es válido
void PonerNTInf(TableroInfinito t, Color color, int n){
    t->celdaActual->bolitas[color] += n;
}

//--------------------------------------------------------------------------
// PROP: Modifica el tablero dado para sacar n bolitas del color dado de la celda 
// actual del tablero, si existe esa cantidad, o falla con BOOM en otro caso,
// PRECOND:
//   el color es válido
//   hay al menos n bolitas en la celda actual en t
void SacarNTInf(TableroInfinito t, Color color, int n){
  if (t->celdaActual->bolitas[color] >= n) {
    t->celdaActual->bolitas[color] -= n;
  } else {
    BOOM("No hay suficientes bolitas");
  }
}

//--------------------------------------------------------------------------
// *Preguntar si hace falta crear celdas intermedias.
// PROP: Modifica el tablero dado para mover n posiciones en la dirección dada a la celda actual
// PRECOND: la dirección dada es válida
void MoverNTInf(TableroInfinito t, Dir dir, int n){
  switch (dir)
  {
  case NORTE:
    t->celdaActual = insertBBNode(t->tablero, t->celdaActual->kx, t->celdaActual->ky + n);
    break;
  case SUR:
    t->celdaActual = insertBBNode(t->tablero, t->celdaActual->kx, t->celdaActual->ky - n);
    break;
  case ESTE:
    t->celdaActual = insertBBNode(t->tablero, t->celdaActual->kx + n, t->celdaActual->ky);
    break;
  case OESTE:
    t->celdaActual = insertBBNode(t->tablero, t->celdaActual->kx - n, t->celdaActual->ky);
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
  return t->celdaActual->bolitas[color];
}

//--------------------------------------------------------------------------
// PROP: Libera toda la memoria del tablero dado.
void LiberarTInf(TableroInfinito t){
  // COMPLETAR
}

//==========================================================================
// Impresión para verificaciones
//==========================================================================
void PrintRepTInf(TableroInfinito t) {
  // COMPLETAR 
  // PISTA: utilizar PrintBB de BiBST
}