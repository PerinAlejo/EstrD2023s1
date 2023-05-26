#include <iostream>
#include "Par.h"
using namespace std;

// ====================================================================================================
// ======================================Ejercicio 1===================================================
// ====================================================================================================
/*
int main() {
    int x = 0;
    int y = 2;
    x = x+y;
}

int main() {
    int x = 0;
    int y = 0;
    while(y < 5) {
        x += y;
        y++;
    }
}

int main() {
    int y = 0;
    bool b = true;
    while(b) {
        y++;
        b = !b;
    }
} */

// ====================================================================================================
// ======================================Ejercicio 2===================================================
// ====================================================================================================

// Proposito: imprime en pantalla todos los codigos ASCII de las letras dadas y su intermedias.
// Precondición: c1 < c2
void printFromTo(char c1, char c2) {
    for(int i = 0; c1 + i <= c2; i++) {
        cout << c1 + i << ", ";
    }
    cout << endl;
}

// Proposito: Calcula el factorial del numero dado.
// Precondición: n >= 0
int fc(int n) {
    int x = 1;
    while(n > 0) {
        x = x * n;
        n--;
    }
    return x;
}

/* Proposito: suma todos los los valores que van desde el primer valor dado al segundo valor dado,
              si son iguales devuelve ese valor */
// Precondición: n <= m
int ft(int n, int m) {
    if (n == m) {
    return n;
    }
    return n + ft(n+1, m);
}

// ====================================================================================================
// ======================================Ejercicio 3===================================================
// ====================================================================================================

int showPar(Par p) {
    cout << "("<< fst (p) << ","<< snd (p) << ")" << endl;
}

// ====================================================================================================
// ======================================Ejercicio 4===================================================
// ====================================================================================================

//Propósito: imprime n veces un string s
void printN(int n, string s) {
    for(int i = 0 ; i < n ; i++){
        cout << s << endl;
    }
}

void printNR(int n, string s) {
    if (n != 0) {
        cout << s << endl;
        printNR(n-1, s)
    }
}

void cuentaRegresiva(int n) {
    for(int i = n ; i != 0 ; i--) {
        cout << i << endl;
    }
    cout << 0 << endl;
}

void cuentaRegresivaR(int n) {
    if (n != 0) {
        cout << n << endl;
        cuentaRegresivaR(n-1)
    }
    cout << 0 << endl;
}


// ====================================================================================================
// =========================================== Main ===================================================
// ====================================================================================================

int main() {
    printN(5,"SOL");
}