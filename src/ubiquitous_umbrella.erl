-module (ubiquitous_umbrella).
-include("ubiquitous_umbrella.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    allocate/0,
    allocate/1,
    allocate_non_group/0,
    allocate_non_group/1,
    hello/0
]).

-define(OPTS, [
    {receive_timeout, timer:seconds(5)}
]).

-define(ATTEMPTS_COUNT, 5).
-define(DEFAULT_TYPE, <<"grass">>).

allocate() ->
    allocate(?DEFAULT_TYPE, ?ATTEMPTS_COUNT).

allocate(Type) ->
    allocate(Type, ?ATTEMPTS_COUNT).

allocate(_Type, 0) ->
    {error, nopokemons};
allocate(Type, Attempts) ->
    Sub = <<"pokemons.group.allocate">>,
    case ?gnat:request(gnat, Sub, Type, ?OPTS) of
        {ok, #{body := <<"[exhausted]">>}} ->
            allocate(Type, Attempts - 1);
        {ok, Resp} ->
            Resp
    end.

allocate_non_group() ->
    allocate_non_group(?DEFAULT_TYPE).

allocate_non_group(Type) ->
    Sub = <<"pokemons.allocate">>,
    case ?gnat:request(gnat, Sub, Type, ?OPTS) of
        {ok, #{body := _Num, reply_to :=  ReplyTo}} ->
            ?gnat:request(gnat, ReplyTo, Type, ?OPTS);
        {error, timeout} ->
            {error, nopokemons}
    end.

hello() ->
    world.
