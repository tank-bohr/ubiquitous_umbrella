-module(ubiquitous_umbrella_app).
-include_lib("kernel/include/logger.hrl").

-behaviour(application).

-export([
    start/2,
    stop/1,
    prep_stop/1
]).

start(_StartType, _StartArgs) ->
    start_http(8080),
    ubiquitous_umbrella_sup:start_link().

stop(State) ->
    ?LOG_DEBUG("Done", [State]),
    ok.

prep_stop(State) ->
    ?LOG_DEBUG("Stopping...", [State]),
    State.

start_cowboy(Port) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", ubiquitous_umbrella_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(uu_http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ).

start_http(Port) ->
    case os:getenv("CONSUL_ADDRESS") of
        false ->
            skip;
        ConsulAddress ->
            start_cowboy(Port),
            register_service(Port, ConsulAddress)
    end.

register_service(Port, ConsulAddress) ->
    {ok, Hostname} = inet:gethostname(),
    ShardNumber = list_to_binary(os:getenv("SHARD_NUMBER")),
    Service = jsx:encode(#{
        <<"ID">> => <<"ubiquitous-umbrella-", ShardNumber/binary>>,
        <<"Name">> => <<"ubiquitous-umbrella">>,
        <<"Address">> => list_to_binary(Hostname),
        <<"Port">> => Port
    }),
    ?LOG_DEBUG("Service is [~s]", [Service]),
    Url = ConsulAddress ++ "/v1/agent/service/register",
    ?LOG_DEBUG("Url is [~s]", [Url]),
    Request = {Url, [], "application/json", Service},
    {ok, {{_, 200, _}, _Headers, _Body}} = httpc:request(put, Request, [], [{body_format, binary}]).
