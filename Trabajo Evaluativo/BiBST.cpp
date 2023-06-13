#include <iostream>
#include <algorithm>
#include "BiBST.h"
using namespace std;


//==========================================================================
// Invariante de representación
//==========================================================================
/* INV.REP.
   * COMPLETAR
*/

//==========================================================================
// Implementación
//==========================================================================
BBNode* findBBNode(BBNode* nodo, int x, int y) { 
  return NULL; // REEMPLAZAR
}

BBNode* insertBBNode(BBNode* nodo, int x, int y) { 
  return NULL; // REEMPLAZAR
}

void LiberarBiBST(BiBST t) { 
  // COMPLETAR
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

