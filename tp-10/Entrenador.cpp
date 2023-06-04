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
        e->pokemon = pokemon;
        return e;
}

string nombreDeEntrenador(Entrenador e){
    return e->nombre;
}

int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e){
    
}




//PRECONDICION: hay n pokemons. n >=1 
Pokemon pokemonNumero(Entrenador e, int n) {
    return e->pokemon[n-1];
}

