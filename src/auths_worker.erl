-module(auths_worker).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([useradd/2, login/2, logout/2, ping/2]).

%% -define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(SERVER, ?MODULE).

-record(state, {db, conn}).

%% API
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%% callback
useradd(Username, Password) ->
    gen_server:call(?MODULE, {useradd, Username, Password}).

login(Username, Password) ->
    gen_server:call(?MODULE, {login, Username, Password}).

logout(Username, Session) ->
    gen_server:call(?MODULE, {logout, Username, Session}).

ping(Username, Session) ->
    gen_server:call(?MODULE, {ping, Username, Session}).

%% init
init(Args) ->
    DB = proplists:get_value(db, Args),
    %DB = #{host => "localhost", username => "auths", password => "auths", database => "auths", timeout => 4000},
    %erlang:send_after(1000, self(), cleanup),
    {ok, Conn} = epgsql:connect(DB),
    {ok, #state{db = DB, conn = Conn}}.

%% handle 
handle_call({useradd, Username, Password}, _From, #state{conn=Conn}=State) ->
    Result = auths_db:useradd(Conn, Username, Password),
    {reply, Result, State};
handle_call({login, Username, Password}, _From, #state{conn=Conn}=State) ->
    Result = auths_db:login(Conn, Username, Password, 60),
    {reply, Result, State};
handle_call({logout, Username, Session}, _From, #state{conn=Conn}=State) ->
    Result = auths_db:logout(Conn, Username, Session),
    {reply, Result, State};
handle_call({ping, Username, Session}, _From, #state{conn=Conn}=State) ->
    Result = auths_db:ping(Conn, Username, Session),
    {reply, Result, State};
handle_call(_, _From, State) ->
    {reply, {error, "wrong message"}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({cleanup}, State) ->
    auths_db:cleanup(State#state.db),
    erlang:send_after(1000, self(), cleanup),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_, State) ->
    ok = epgsql:close(State#state.conn),
    {shutdown, ok}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
