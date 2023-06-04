#include "Entrenador.h"
#include "Pokemon.h"

struct EntrenadorSt {
    string nombre;
    Pokemon* pokemon;
    int cantPokemon;
}

Entrenador conEntrenador(string nombre, int cantidad, Pokemon* pokemon) {
        EntrenadorSt* e = new EntrenadorSt;
        e->nombre = nombre;
        e->cantPokemon = cantidad;
        e->pokemon = pokemon[cantidad];
        return e;
}

string nombreDeEntrenador(Entrenador e){
    return e->nombre;
}

int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e){
    int count = 0
    for (int i = 0; i < e->cantPokemon; i++){
        int p = e->pokemon[i];
        count = count + (p->tipo == tipo) ? 1 : 0;
    }
    return count;
}

//PRECONDICION: hay n pokemons. n >=1 
Pokemon pokemonNumero(Entrenador e, int n) {
    return e->pokemon[n-1];
}

bool leGanaATodos(Entrenador e1, Entrenador e2){
    bool leGanaATodos = false; 
    while(not leGanaATodos && i <= e1->cantPokemon) {
        int i = 1;
        leGanaATodos = pokemonLeGanaATodos(pokemonNumero(e1, i++), e2);
    }
    return leGanaATodos;
}

bool pokemonLeGanaATodos(Pokemon p, Entrenador e){
    bool PokeLeGanaATodos = true;
    while (PokeLeGanaATodos && i <= e->cantPokemon) {
        int i = 1;
        PokeLeGanaATodos = superaA(p, pokemonNumero(e, i++));
    }
    return PokeLeGanaATodos;
}

