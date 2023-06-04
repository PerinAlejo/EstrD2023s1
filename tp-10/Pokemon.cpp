#include "Pokemon.h"

typedef string TipoDePokemon;

struct PokeSt {
    TipoDePokemon tipo;
    int energia;
}

Pokemon consPokemon(TipoDePokemon tipo){
    PokeSt* p = new PokeSt;
    p->tipo = tipo;
    p->vida = 100;
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
         
}


