#include "Pokemon.h"
#include <iostream>
using namespace std;



struct PokeSt {
    TipoDePokemon tipo;
    int energia;
} ;

Pokemon consPokemon(TipoDePokemon tipo){
    PokeSt* p = new PokeSt;
    p->tipo = tipo;
    p->energia = 100;
    return p; 
}

TipoDePokemon tipoDePokemon(Pokemon p) {
    return p->tipo;
}

int energia(Pokemon p) {
    return p->energia;
}

void perderEnergia(int energia, Pokemon p) {
    if (p->energia < energia) {
        p->energia = 0;
    } else {
        p->energia = p->energia - energia;
    }
}

bool superaA(Pokemon p1, Pokemon p2) {
         bool resultado; 
         if (p1->tipo == "agua") {
            (p2->tipo == "fuego") ? true : false;
         }
         if (p1->tipo == "fuego") {
            (p2->tipo == "planta") ? true : false;
         }
         if (p1->tipo == "planta") {
            (p2->tipo == "agua") ? true : false;
         }
         return resultado;
}


