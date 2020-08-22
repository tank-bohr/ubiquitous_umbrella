-module(ubiquitous_umbrella_app).
-include_lib("kernel/include/logger.hrl").

-behaviour(application).

-export([
    start/2,
    stop/1,
    prep_stop/1
]).

start(_StartType, _StartArgs) ->
    ubiquitous_umbrella_sup:start_link().

stop(State) ->
    ?LOG_DEBUG("Done", [State]),
    ok.

prep_stop(State) ->
    ?LOG_DEBUG("Stopping...", [State]),
    State.
