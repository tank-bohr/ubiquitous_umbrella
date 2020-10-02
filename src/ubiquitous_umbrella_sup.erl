-module (ubiquitous_umbrella_sup).
-include("supervisor_spec.hrl").
-include("ubiquitous_umbrella.hrl").

-export([start_link/0]).

-behaviour(supervisor).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

%% @private
init({}) ->
    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, children()}}.

children() ->
    children(os:getenv("SHARD_NUMBER"), os:getenv("SHARDS_COUNT")).

children(ShardNumber, ShardsCount) when ShardNumber =:= false, ShardsCount =:= false ->
    [gnat_server_spec()];
children(ShardNumberStr, ShardsCountStr) ->
    ShardNumber = list_to_integer(ShardNumberStr),
    ShardsCount = list_to_integer(ShardsCountStr),
    Args = [{ShardNumber, ShardsCount}],
    [
        gnat_server_spec(),
        ubiquitous_umbrella_server:child_spec(Args)
    ].

gnat_server_spec() ->
    ?WORKER(?gnat_connection_supervisor, [gnat_settings(), [{name, gnat_connection_supervisor}]]).

gnat_settings() ->
    {ok, ConnectionSettings} = application:get_env(ubiquitous_umbrella, gnat_connection_settings),
    #{
        name                => gnat,
        connection_settings => ConnectionSettings
    }.
