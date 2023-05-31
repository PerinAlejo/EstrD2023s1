#include <iostream>
#include "Par.h"
#include "Fraccion.h"
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
        printNR(n-1, s);
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
        cuentaRegresivaR(n-1);
    } else {
        cout << 0 << endl;
    }  
}

//Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
void desdeCeroHastaN(int n) {
    for (int i = 0 ; i <= n ; i++) {
        cout << i << endl;
    }
}

void desdeCeroHastaNR(int n) {
    if (n < 0) {
        return;
    }
    desdeCeroHastaN(n - 1);
    cout << n << endl;
}


//Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++)
int mult(int n, int m) {
    int r = 0;
    for(int i = n ; n > 0 ; n--) {
        r = r + m;
    }
    return r;
}

int multR(int n, int m) {
    int r = 0;
    if (n != 0) {
        r = multR(n-1, m) + m ;
    } 
    return r;
}

// Propósito: imprime los primeros n char del string s, separados por un salto de línea.
// Precondición: el string tiene al menos n char.
void primerosN(int n, string s) {
    for (int i = 0; i < n ; i++){
        cout << s[i] << endl;
    }
}
void primerosNR(int n, string s) {
    if (n > 0) {
        primerosNR(n-1, s);
        cout << s[n-1] << endl;
    }
}

// Propósito: indica si un char c aparece en el string s.
bool pertenece(char c, string s){
    bool r = false;
    int i  = 0; 
    while (not r && s[i] != '\0') {     
        r = (c == s[i]);
        i++ ;
    }
    return r ;
}

bool perteneceR(char c, string s) {
    if (s.empty()) {
        return false;
    }
    if (s[0] == c) {
        return true;
    }
    return pertenece(c, s.substr(1));
}

//Propósito: devuelve la cantidad de apariciones de un char c en el string s.
int apariciones(char c, string s) {
    int count = 0;
    for (int i=0; s[i] != '\0'; i++) {
        if (c==s[i]){
            count++;
        }
    }
    return count;
}

int aparicionesR(char c, string s) { 
    if(s.empty()) {
        return 0;
    }
    int count = (s[0] == c) ? 1 : 0;
    return count + apariciones(c, s.substr(1));
}


// ====================================================================================================
// =========================================== Main ===================================================
// ====================================================================================================

int main() {
   Fraccion f = simplificada(consFraccion(10,20));
   cout << "Numerador: " << numerador(f) << endl;
   cout << "Denominador: " << denominador(f) << endl;
}