-module(ubiquitous_umbrella_handler).
-include_lib("kernel/include/logger.hrl").

-behaviour(cowboy_handler).
-export([init/2]).

-define(RESP_HEADERS, #{
    <<"content-type">> => <<"application/json">>
}).

init(Req, State) ->
    Type = proplists:get_value(<<"type">>, cowboy_req:parse_qs(Req), <<"grass">>),
    Reply = case ubiquitous_umbrella_server:allocate(Type) of
        exhausted ->
            cowboy_req:reply(404, ?RESP_HEADERS, <<"{}">>, Req);
        {found, Found} ->
            cowboy_req:reply(200, ?RESP_HEADERS, jsx:encode(Found), Req)
    end,
    {ok, Reply, State}.
