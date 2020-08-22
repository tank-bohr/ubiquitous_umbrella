-ifndef(SUPERVISOR_SPEC_HRL).
-define(SUPERVISOR_SPEC_HRL, true).

-define(WORKER(Mod, Args),       ?CHILD(worker, Mod, Args, [])).
-define(WORKER(Mod, Args, Opts), ?CHILD(worker, Mod, Args, Opts)).

-define(SUPERVISOR(Mod, Args),       ?CHILD(supervisor, Mod, Args, [])).
-define(SUPERVISOR(Mod, Args, Opts), ?CHILD(supervisor, Mod, Args, Opts)).

-define(CHILD(Type, Mod, Args, Opts), #{
    id       => proplists:get_value(id, Opts, Mod),
    start    => {Mod, proplists:get_value(id, Opts, start_link), Args},
    restart  => proplists:get_value(restart, Opts, permanent),
    shutdown => proplists:get_value(shutdown, Opts, 5000),
    type     => Type,
    modules  => [Mod]
}).

-endif.
