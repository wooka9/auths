-module(auths).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([useradd/2, login/2, logout/2]).

-define(SERVER, ?MODULE).

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
	_Result = auths_db:create(?MODULE),
	%erlang:send_after(60000, self(), {cleanup}),
	%{reply, Result, State};
	{ok, []}.

%% handle
handle_call({useradd, Username, Password}, _From, State) ->
	Result = auths_db:useradd(?MODULE, Username, Password),
	{reply, Result, State};
handle_call({login, Username, Password}, _From,  State) ->
	Result = auths_db:login(?MODULE, Username, Password, 60),
	{reply, Result, State};
handle_call({logout, Username, Session}, _From, State) ->
	Result = auths_db:logout(?MODULE, Username, Session),
	{reply, Result, State};
handle_call(_, _From, State) ->
	{reply, wrong_message, State}.

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
