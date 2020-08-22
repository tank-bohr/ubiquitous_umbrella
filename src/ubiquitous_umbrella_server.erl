-module(ubiquitous_umbrella_server).
-include("ubiquitous_umbrella.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    child_spec/1,
    start_link/1
]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-define(SERVER, ?MODULE).

-record(state, {
    name    :: atom(),
    topic   :: binary(),
    backoff :: backoff:backoff(),
    sub     :: undefined | non_neg_integer()
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

start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% @private
init(#{name := Name, topic := Topic}) ->
    Backoff = backoff:init(2, 10),
    State = #state{name = Name, topic = Topic, backoff = Backoff},
    {ok, State, timer:seconds(1)}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(timeout, #state{name = Name} = State) ->
    case whereis(Name) of
        Pid when is_pid(Pid) ->
            subscribe(State);
        _ ->
            ?LOG_WARNING("Gnat is not ready yet"),
            {Timeout, Backoff} = backoff:fail(State#state.backoff),
            {noreply, State#state{backoff = Backoff}, timer:seconds(Timeout)}
    end;
handle_info({msg, #{topic := Topic, body := Body} = Msg}, #state{name = Name} = State) ->
    ?LOG_INFO("Message [~p] received from topic [~s]", [Body, Topic]),
    case Msg of
        #{reply_to := nil} ->
            ?LOG_INFO("No reply needed");
        #{reply_to := ReplyTo} ->
            ok = ?gnat:pub(Name, ReplyTo, <<"Wazzup">>, [{reply_to, <<"noreply">>}])
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

subscribe(#state{name = Name, topic = Topic} = State) ->
    case ?gnat:sub(Name, self(), Topic) of
        {ok, Sub} ->
            {_, Backoff} = backoff:succeed(State#state.backoff),
            {noreply, State#state{backoff = Backoff, sub = Sub}};
        Error ->
            ?LOG_ERROR("Could'n subscribe to topic [~s] due to ~p", [Topic, Error]),
            {Timeout, Backoff} = backoff:fail(State#state.backoff),
            {noreply, State#state{backoff = Backoff}, timer:seconds(Timeout)}
    end.
