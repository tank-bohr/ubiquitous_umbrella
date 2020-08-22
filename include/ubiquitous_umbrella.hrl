-ifndef(UBIQUITOUS_UMBRELLA_ERL).
-define(UBIQUITOUS_UMBRELLA_ERL, true).

-define(gnat, 'Elixir.Gnat').
-define(gnat_connection_supervisor, 'Elixir.Gnat.ConnectionSupervisor').

-define(DEFAULT_GNAT_SETTINGS, #{
    name => gnat,
    connection_settings => [
        #{host => "127.0.0.1", port => 4222}
    ],
    topic => <<"pants.*">>
}).

-endif.
