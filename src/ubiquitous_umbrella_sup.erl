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
    GnatSettings = gnat_settings(),
    Children = [
        ?WORKER(?gnat_connection_supervisor, [GnatSettings, [
            {name, gnat_connection_supervisor}
        ]]),
        ubiquitous_umbrella_server:child_spec([GnatSettings])
    ],
    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, Children}}.

gnat_settings() ->
    application:get_env(ubiquitous_umbrella, gnat_settings, ?DEFAULT_GNAT_SETTINGS).
