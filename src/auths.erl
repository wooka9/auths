-module(auths).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([useradd/2, login/2, logout/2]).

-define(SERVER, ?MODULE).

%% -record(state, {db_users, db_sessions}).

%% -import(auths_db, [create/1, useradd/2, login/3, logout/2]).

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

%% init
init([]) ->
	%DB_users = list_to_atom(atom_to_list(?MODULE) ++ "_users"),
	%DB_sessions = list_to_atom(atom_to_list(?MODULE) ++ "_sessions"),
	%auths_db:create({DB_users, DB_sessions}),
	auths_db:create({auths_users, auth_sessions}),
	%erlang:send_after(60000, self(), {cleanup}),
	%{reply, Result, State};
	{ok, []}.

%% handle
handle_call({useradd, Username, Password}, _From, State) ->
	Result = auths_db:useradd({auths_users, auth_sessions}, Username, Password),
	{reply, Result, State};
handle_call({login, Username, Password}, _From,  State) ->
	Result = auths_db:login({auths_users, auth_sessions}, Username, Password, 60),
	{reply, Result, State};
handle_call({logout, Username, Session}, _From, State) ->
	Result = auths_db:logout({auths_users, auth_sessions}, Username, Session),
	{reply, Result, State};
handle_call(_, _From, State) ->
	{reply, {error, "wrong message"}, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

%handle_info({cleanup}, State) ->
%	auths_db:cleanup(?MODULE),
%	erlang:send_after(60000, self(), {cleanup}),
%	{noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_, _) ->
    {shutdown,ok}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
