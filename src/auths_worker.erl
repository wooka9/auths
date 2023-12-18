-module(auths_worker).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%-export([useradd/2, login/2, logout/2, ping/2]).

%% -define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(SERVER, ?MODULE).

-record(state, {db, connection}).

%% API
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%% init
init(WorkerArgs) ->
    DB = proplists:get_value(db, WorkerArgs),
    erlang:send_after(1000, self(), check),
    {ok, Connection} = epgsql:connect(DB),
    {ok, #state{db = DB, connection = Connection}}.

%% handle
%handle_call({Action, Args}, _From, #state{connection = Connection} = State) ->
%    ActionAllowed = [useradd, login, logout, ping, close],
%    case lists:member(Action, ActionAllowed) andalso is_list(Args) andalso length(Args) =:= 2 of
%        true ->
%            Result = apply(auths_db, Action, [Connection, Args]),
%            {reply, Result, State};
%        _ -> {reply, {error, "wrong message"}, State}
%    end.

handle_call({useradd, Args}, _From, #state{connection = Connection} = State) ->
    Result = auths_db:useradd(Connection, Args),
    {reply, Result, State};
handle_call({login, Args}, _From, #state{connection = Connection} = State) ->
    Result = auths_db:login(Connection, Args),
    {reply, Result, State};
handle_call({logout, Args}, _From, #state{connection = Connection} = State) ->
    Result = auths_db:logout(Connection, Args),
    {reply, Result, State};
handle_call({ping, Args}, _From, #state{connection = Connection} = State) ->
    Result = auths_db:ping(Connection, Args),
    {reply, Result, State};
handle_call({close, Args}, _From, #state{connection = Connection} = State) ->
    Result = auths_db:close(Connection, Args),
    {reply, Result, State};
handle_call(_, _From, State) ->
    {reply, {error, "wrong message"}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({check}, #state{db = DB, connection = ConnectionNow} = State) ->
    case epgsql:ping(ConnectionNow) of
        ok ->
            erlang:send_after(1000, self(), check),
            {noreply, State};
        _ ->
            {ok, ConnectionNew} = epgsql:connect(DB),
            io:format("~nRestarting Connection to DB.~n", []),
            erlang:send_after(1000, self(), check),
            {noreply, #state{db = DB, connection = ConnectionNew}}
    end;
handle_info({cleanup}, #state{connection = Connection} = State) ->
    auths_db:cleanup(Connection),
    erlang:send_after(1000, self(), cleanup),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_, #state{connection = Connection}) ->
    ok = epgsql:close(Connection),
    {shutdown, ok}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
