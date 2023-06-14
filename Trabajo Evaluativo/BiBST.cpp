#include <iostream>
#include <algorithm>
#include "BiBST.h"
using namespace std;

//==========================================================================
// Invariante de representación
//==========================================================================
/* INV.REP.
   - int bolitas debe tener numeros positivos.

   NOTAS.
   - AZUL NEGRO ROJO VERDE. Posiciones de los colores.
*/

//==========================================================================
// Implementación
//==========================================================================

//Devuelve el cuadrante en donde buscar o insertar un nodo.
//Prec: Se debe cumplir que (x != nodo->kx || y != nodo->yx)
Cuadrante cuandranteCorrespondiente(BBNode* nodo, int x, int y) {
    if (x > nodo->kx && y > nodo->ky) {
      return NE;
    } else if (x > nodo->kx && y <= nodo->ky) {
      return SE;
    } else if (x <= nodo->kx && y > nodo->ky) {
      return NO;
    } else if (x <= nodo->kx && y <= nodo->ky) {
      return SO;
    }
}

//devuelve el nodo del árbol que posee las claves dadas, o NULL si no existe tal nodo.
BBNode* findBBNode(BBNode* nodo, int x, int y) { 
  if (nodo != EMPTYBB) {
    if (x != nodo->kx || y != nodo->ky) {
      return findBBNode(nodo->hijo[cuandranteCorrespondiente(nodo, x, y)], x, y);
    } else {
      return nodo;
    }
  } else {
    return NULL;
  } 
}

//devuelve el nodo del árbol con las claves dadas, si el nodo no existe, lo crea y lo inserta 
//adecuadamente en el árbol.
BBNode* insertBBNode(BBNode* nodo, int x, int y) {
  if(nodo == EMPTYBB) {
      BBNode* nuevoNodo = new BBNode;
      nuevoNodo->kx = x;
      nuevoNodo->ky = y;
      nodo = nuevoNodo;
      return nodo;
  } else {
    if (x != nodo->kx || y != nodo->ky) {
      if (nodo->hijo[cuandranteCorrespondiente(nodo, x, y)] == EMPTYBB) {
        BBNode* nuevoNodo = new BBNode;
        nuevoNodo->kx = x;
        nuevoNodo->ky = y;
        nodo->hijo[cuandranteCorrespondiente(nodo, x, y)] = nuevoNodo;
        return nuevoNodo;
      } else {
        return insertBBNode(nodo->hijo[cuandranteCorrespondiente(nodo, x, y)], x, y);
      }
    } else {
      return nodo;
    }  
  } 
}

void LiberarBiBST(BiBST t) {
  for (int i = 0; i <= 3; i++) {
    if (t->hijo[i] != EMPTYBB) {
      LiberarBiBST(t->hijo[i]);
    }
  }
  delete t->hijo;
  delete t;
}

//==========================================================================
// Impresión para verificaciones
//==========================================================================
void PrintBBNode(BBNode* t, int tab) {
  if (t == NULL) { return; }
  INDENT(tab)
  cout << "  (" << t->kx << "," << t->ky << "): ";
  PRINTCOLORN(AZUL , t->bolitas[AZUL ]); 
  cout << ", "; PRINTCOLORN(NEGRO, t->bolitas[NEGRO]); 
  cout << ", "; PRINTCOLORN(ROJO , t->bolitas[ROJO ]); 
  cout << ", "; PRINTCOLORN(VERDE, t->bolitas[VERDE]); 
  cout << endl;
  PrintBBNode(t->hijo[NE], ++tab);
  PrintBBNode(t->hijo[SE], tab);
  PrintBBNode(t->hijo[NO], tab);
  PrintBBNode(t->hijo[SO], tab);
}

void PrintBB(BiBST t) {
  cout << "BiBST:" << endl;
  PrintBBNode(t, 0);
}

