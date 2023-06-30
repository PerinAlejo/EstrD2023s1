#include <iostream>
#include <algorithm>
#include "BiBST.h"
using namespace std;

//==========================================================================
// Invariante de representación
//==========================================================================
/* INV.REP.
   - Los elementos de bolitas deben ser >= 0.
   - Los decendientes del BBNode deben cumplir con las siguientes condiciones:
      Para el cuadrante NE x > kx && y > ky;
      Para el cuadrante SE x > kx && y <= ky;
      Para el cuadrante NO x <= kx && y > ky;
      Para el cuadrante SO x <= kx && y <= ky;
    donde x es el kx e y es el ky de los decendientes del arbol.
    - El arbol no puede tener nodos con claves repetidas.
    - Cada BBNode debe cumplir con las invariantes de BiBST.

   NOTAS.
   - AZUL NEGRO ROJO VERDE. Posiciones de los colores.
   - NE    SE    NO   SO.   Posiciones de los cuadrantes.
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

BBNode* crearNuevoNodo (int x, int y) {
  BBNode* nuevoNodo = new BBNode;
  nuevoNodo->kx = x;
  nuevoNodo->ky = y;
  return nuevoNodo;
}

//devuelve el nodo del árbol con las claves dadas, si el nodo no existe, lo crea y lo inserta 
//adecuadamente en el árbol.
BBNode* insertBBNode(BBNode* nodo, int x, int y) {
  if(nodo == EMPTYBB) {
      nodo = crearNuevoNodo(x,y);
      return nodo;
  } else {
    if (x != nodo->kx || y != nodo->ky) {
      if (nodo->hijo[cuandranteCorrespondiente(nodo, x, y)] == EMPTYBB) {
        nodo->hijo[cuandranteCorrespondiente(nodo, x, y)] = crearNuevoNodo(x,y);
        return nodo->hijo[cuandranteCorrespondiente(nodo, x, y)];
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

