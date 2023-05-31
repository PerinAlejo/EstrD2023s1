#include "fraccion.h"
// ====================================================================================================
// ======================================Ejercicio 5===================================================
// ====================================================================================================

// Propósito: construye una fraccion
// Precondición: el denominador no es cero
Fraccion consFraccion(int numerador, int denominador){
    Fraccion f; 
    f.numerador   = numerador;
    f.denominador = denominador;
    return f; 
}

// Propósito: devuelve el numerador
int numerador(Fraccion f){
    return f.numerador;
}

// Propósito: devuelve el denominador
int denominador(Fraccion f){
    return f.denominador;
}

// Propósito: devuelve el resultado de hacer la división
float division(Fraccion f){
    return f.numerador / f.denominador; 
}

// Propósito: devuelve una fracción que resulta de multiplicar las fracciones
// (sin simplificar)
Fraccion multF(Fraccion f1, Fraccion f2){
    Fraccion result;
    result.numerador = f1.numerador * f2.numerador;
    result.denominador = f1.denominador * f2.denominador;
    return result;
}

// Calcula el máximo común divisor entre el numerador y el denominador
int maximoComunDivisor(Fraccion f) {
    int mcd = 1; // Máximo común divisor inicializado en 1

    // Calcula el máximo común divisor entre el numerador y el denominador
    for (int i = 1; i <= f.numerador && i <= f.denominador; i++) {
        if (f.numerador % i == 0 && f.denominador % i == 0) {
            mcd = i;
        }
    }
    return mcd;
}

// Propósito: devuelve una fracción que resulta
// de simplificar la dada por parámetro
Fraccion simplificada(Fraccion f){
    int mcd = maximoComunDivisor(f);

    Fraccion result;
    result.numerador = f.numerador / mcd;
    result.denominador = f.denominador / mcd;
    return result;
}

// Propósito: devuelve la primera componente
Fraccion sumF(Fraccion f1, Fraccion f2) {
    Fraccion result;
    result.numerador = f1.numerador * f2.denominador + f2.numerador * f1.denominador;
    result.denominador = f1.denominador * f2.denominador;
    return result;
}


