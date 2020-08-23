-module (ubiquitous_umbrella).
-include("ubiquitous_umbrella.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    allocate_grass_pokemon/0,
    allocate_pokemon/1,
    hello/0
]).

allocate_grass_pokemon() ->
    allocate_pokemon(<<"grass">>).

allocate_pokemon(Type) ->
    ?gnat:request(gnat, <<"pokemons.allocate">>, Type, [
        {receive_timeout, timer:seconds(5)}
    ]).

hello() ->
    world.
