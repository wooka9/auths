-module(auths).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([useradd/2, login/2, logout/2, ping/2]).

-define(SERVER, ?MODULE).
%% -record(dbs, {db_users, db_sessions}).

%% API
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
	DB_users = list_to_atom(atom_to_list(?MODULE) ++ "_users"),
	DB_sessions = list_to_atom(atom_to_list(?MODULE) ++ "_sessions"),
	DBs={DB_users, DB_sessions},
	auths_db:create(DBs),
	erlang:send_after(1000, self(), cleanup),
	{ok, DBs}.

%% handle
handle_call({useradd, Username, Password}, _From, State) ->
	Result = auths_db:useradd(State, Username, Password),
	{reply, Result, State};
handle_call({login, Username, Password}, _From,  State) ->
	Result = auths_db:login(State, Username, Password, 60),
	{reply, Result, State};
handle_call({logout, Username, Session}, _From, State) ->
	Result = auths_db:logout(State, Username, Session),
	{reply, Result, State};
handle_call({ping, Username, Session}, _From, State) ->
	Result = auths_db:ping(State, Username, Session),
	{reply, Result, State};
handle_call(_, _From, State) ->
	{reply, {error, "wrong message"}, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({cleanup}, State) ->
	auths_db:cleanup(State),
	erlang:send_after(1000, self(), cleanup),
	{noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_, _) ->
    {shutdown,ok}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
