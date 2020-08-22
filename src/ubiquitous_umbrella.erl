-module (ubiquitous_umbrella).
-include("ubiquitous_umbrella.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    req/2,
    hello/0
]).

req(Topic, Payload) ->
    ?gnat:request(gnat, Topic, Payload).

hello() ->
    world.
