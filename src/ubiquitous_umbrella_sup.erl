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
    Children = [
        ?WORKER(?gnat_connection_supervisor, [gnat_settings(), [
            {name, gnat_connection_supervisor}
        ]]),
        ubiquitous_umbrella_server:child_spec([{1, 3}])
    ],
    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, Children}}.

gnat_settings() ->
    {ok, ConnectionSettings} = application:get_env(ubiquitous_umbrella, gnat_connection_settings),
    #{
        name                => gnat,
        connection_settings => ConnectionSettings
    }.
