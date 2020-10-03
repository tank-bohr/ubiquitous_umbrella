-module(ubiquitous_umbrella_app).
-include_lib("kernel/include/logger.hrl").

-behaviour(application).

-export([
    start/2,
    stop/1,
    prep_stop/1
]).

start(_StartType, _StartArgs) ->
    start_cowboy(),
    ubiquitous_umbrella_sup:start_link().

stop(State) ->
    ?LOG_DEBUG("Done", [State]),
    ok.

prep_stop(State) ->
    ?LOG_DEBUG("Stopping...", [State]),
    State.

start_cowboy() ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", ubiquitous_umbrella_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(uu_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ).
