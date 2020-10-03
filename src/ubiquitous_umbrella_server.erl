-module(ubiquitous_umbrella_server).
-include("ubiquitous_umbrella.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([
    child_spec/1,
    start_link/1,
    allocate/1
]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    handle_continue/2
]).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).
-define(MATCH_HEAD(Type), #pokemon{
    name = '$1',
    type = Type,
    state = deallocated,
    _ = '_'
}).
-define(WITH_TYPE(Type), [{
    ?MATCH_HEAD(Type), %% MatchHead
    [],                %% Guards
    ['$1']             %% Result
}]).
-define(COUNT_WITH_TYPE(Type), [{
    ?MATCH_HEAD(Type), %% MatchHead
    [],                %% Guards
    [true]             %% Result
}]).
-define(ALLOCATION_TIME_SECONDS, 600).

-record(state, {
    shard_number     :: non_neg_integer(),
    shards_count     :: non_neg_integer(),
    backoff          :: backoff:backoff(),
    callback_subject :: binary(),
    id               :: binary(),
    sub              :: undefined | non_neg_integer()
}).

-record(pokemon, {
    name                :: binary(),
    type                :: binary(),
    state = deallocated :: allocated | deallocated
}).

child_spec(Args) ->
    #{
        id       => ?SERVER,
        start    => {?MODULE, start_link, Args},
        restart  => permanent,
        shutdown => 5000,
        type     => worker,
        modules  => [?MODULE]
    }.

start_link(Arg) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Arg, []).

allocate(Type) ->
    gen_server:call(?SERVER, {allocate, Type}).

%% @private
init({ShardNumber, ShardsCount}) ->
    Id = generate_id(),
    State = #state{
        id = Id,
        backoff = backoff:init(2, 10),
        shard_number = ShardNumber,
        shards_count = ShardsCount,
        callback_subject = <<"reply_to.", Id/binary>>
    },
    ets:new(?TAB, [bag, public, named_table, {keypos, #pokemon.name}]),
    {ok, State, {continue, populate_pokemns}}.

%% @private
handle_call({allocate, Type}, _From, #state{id = Id} = State) ->
    Reply = case select_pokemon(Type) of
        not_found ->
            ?LOG_DEBUG("Couldn't find pokemon with type [~p]", [Type]),
            exhausted;
        Pokemon ->
            update_pokemon_state(Pokemon, allocated),
            ?LOG_DEBUG("Pokemon allocated [~s]", [Pokemon]),
            {ok, TRef} = timer:send_after(timer:seconds(?ALLOCATION_TIME_SECONDS), self(), {deallocate, Pokemon}),
            ?LOG_DEBUG("Deallocate timer started [~p]", [TRef]),
            {found, #{pokemon => Pokemon, shard_id => Id}}
    end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_continue(populate_pokemns, #state{shard_number = ShardNumber, shards_count = ShardsCount} = State) ->
    Path = filename:join(code:priv_dir(ubiquitous_umbrella), "pokemons.json"),
    {ok, Bin} = file:read_file(Path),
    Data = jsx:decode(Bin, [return_maps, {labels, existing_atom}]),
    ok = lists:foreach(fun(#{name := Name, type := Types}) ->
        Shard = erlang:phash2(Name, ShardsCount),
        case Shard of
            ShardNumber ->
                ets:insert(?TAB, [#pokemon{name = Name, type = Type} || Type <- lists:usort(Types)]);
            _ ->
                % ?LOG_DEBUG("Skip pokemon [~s]", [Name])
                skip
        end
    end, Data),
    {noreply, State, timer:seconds(1)}.

%% @private
handle_info(timeout, State) ->
    try_subscribe(State);
handle_info({msg, Msg}, #state{sub = Sub, callback_subject = CallbackSubject} = State) ->
    handle_gnat_message(Msg, Sub, CallbackSubject),
    {noreply, State};
handle_info({deallocate, Pokemon}, State) ->
    deallocate_pokemon(Pokemon),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

try_subscribe(State) ->
    case whereis(gnat) of
        Pid when is_pid(Pid) ->
            subscribe(State);
        _ ->
            ?LOG_WARNING("Gnat is not ready yet"),
            {Timeout, Backoff} = backoff:fail(State#state.backoff),
            {noreply, State#state{backoff = Backoff}, timer:seconds(Timeout)}
    end.

subscribe(#state{callback_subject = CallbackSubject} = State) ->
    {ok, GnatSubject} = application:get_env(ubiquitous_umbrella, gnat_subject),
    case ?gnat:sub(gnat, self(), GnatSubject) of
        {ok, Sub} ->
            {ok, GnatGroup} = application:get_env(ubiquitous_umbrella, gnat_group),
            {ok, _} = ?gnat:sub(gnat, self(), GnatGroup, [{queue_group, <<"group">>}]),
            {ok, _} = ?gnat:sub(gnat, self(), CallbackSubject),
            ?LOG_DEBUG("Subscription success"),
            {_, Backoff} = backoff:succeed(State#state.backoff),
            {noreply, State#state{backoff = Backoff, sub = Sub}};
        Error ->
            ?LOG_ERROR("Could'n subscribe due to ~p", [Error]),
            {Timeout, Backoff} = backoff:fail(State#state.backoff),
            {noreply, State#state{backoff = Backoff}, timer:seconds(Timeout)}
    end.

handle_gnat_message(#{topic := Topic, body := Body, sid := Sid} = Msg, Sub, CallbackSubject) ->
    ?LOG_INFO("Message [~p] received from topic [~s]", [Body, Topic]),
    case Msg of
        #{reply_to := nil} ->
            ?LOG_INFO("No reply needed");
        #{reply_to := ReplyTo} when Sub =:= Sid ->
            check_pokemon(Body, ReplyTo, CallbackSubject);
        #{reply_to := ReplyTo} ->
            allocate_pokemon(Body, ReplyTo, CallbackSubject)
    end.

check_pokemon(Type, ReplyTo, CallbackSubject) ->
    case ets:select_count(?TAB, ?COUNT_WITH_TYPE(Type)) of
        0 ->
            ?LOG_DEBUG("Empty for [~p]", [Type]);
        Number ->
            ?gnat:pub(gnat, ReplyTo, integer_to_binary(Number), [{reply_to, CallbackSubject}])
    end.

allocate_pokemon(Type, ReplyTo, CallbackSubject) ->
    case select_pokemon(Type) of
        not_found ->
            ?LOG_DEBUG("Couldn't find pokemon with type [~p]", [Type]),
            ok = ?gnat:pub(gnat, ReplyTo, <<"[exhausted]">>, [{reply_to, CallbackSubject}]);
        Pokemon ->
            update_pokemon_state(Pokemon, allocated),
            ?LOG_DEBUG("Pokemon allocated [~s]", [Pokemon]),
            {ok, TRef} = timer:send_after(timer:seconds(?ALLOCATION_TIME_SECONDS), self(), {deallocate, Pokemon}),
            ?LOG_DEBUG("Deallocate timer started [~p]", [TRef]),
            ok = ?gnat:pub(gnat, ReplyTo, Pokemon, [{reply_to, CallbackSubject}])
    end.

deallocate_pokemon(Pokemon) ->
    update_pokemon_state(Pokemon, deallocated).

update_pokemon_state(Pokemon, State) when State =:= allocated; State =:= deallocated ->
    Objects = ets:lookup(?TAB, Pokemon),
    true = ets:delete(?TAB, Pokemon),
    true = ets:insert(?TAB, [O#pokemon{state = State} || O <- Objects]).

select_pokemon(Type) ->
    case ets:select(?TAB, ?WITH_TYPE(Type)) of
        [] ->
            not_found;
        Pokemons ->
            get_random_element(Pokemons)
    end.

get_random_element(List) -> get_random_element(List, 1, rand:uniform(length(List))).

get_random_element([Elem|_], Current, Index) when Current =:= Index -> Elem;
get_random_element([_|Rest], Current, Index) -> get_random_element(Rest, Current + 1, Index).

generate_id() ->
    string:lowercase(base64:encode(crypto:strong_rand_bytes(16))).
