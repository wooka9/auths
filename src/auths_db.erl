-module(auths_db).
-export([create/1, useradd/3, login/4, logout/3, ping/3, cleanup/1]).

create(DBs) ->
	{DB_users, DB_sessions} = DBs,
	% case ets:whereis(DB)
	ets:new(DB_users, [named_table, public]),
	ets:new(DB_sessions, [named_table, public]),
	ok.

useradd(DBs, Username, Password) ->
	{DB_users, _DB_sessions} = DBs,
	case ets:lookup(DB_users, Username) of
		[{Username, _Password}] ->
			{error, "username already exists"};
		_ ->
			case is_integer(Username) or is_integer(Password) of
				false ->
					ets:insert(DB_users, {Username, Password}),
					{ok, "username and password added"};
				true ->
					{error, "username or password format invalid"}
			end
	end.

login(DBs, Username, Password, ExpireIn) ->
	{DB_users, DB_sessions} = DBs,
	case ets:lookup(DB_users, Username) of
		[{Username, Password}] ->
			TimeNow = erlang:system_time(seconds),
			Session = erlang:phash2({node(), erlang:system_time()}),
			ets:insert(DB_sessions, {Session, Username, TimeNow + ExpireIn}),
			{ok, "session added", #{<<"session">> => Session}};
		_ ->
			{error, "username or password are incorrect"}
	end.

logout(DBs, Username, Session) ->
	{_DB_users, DB_sessions} = DBs,
	case ets:lookup(DB_sessions, Session) of
		[{Session, Username, _ExpireTime}] ->
			ets:delete(DB_sessions, Session),
			{ok, "sessions closed"};
		_ ->
			{error, "session does not exists"}
	end.

ping(DBs, Username, Session) ->
	{_DB_users, DB_sessions} = DBs,
	case ets:lookup(DB_sessions, Session) of
		[{Session, Username, _ExpireTime}] ->
			{ok, "pong"};
		_ ->
			{error, "session does not exists"}
	end.

cleanup(DBs) ->
	{_DB_users, DB_sessions} = DBs,
	TimeNow = erlang:system_time(seconds),
	MatchSpec = ets:fun2ms(fun({_, _, ExpireTime}) when ExpireTime < TimeNow -> true end),
	ets:select_delete(DB_sessions, MatchSpec),
	{ok, "cleanup seccessful"}.
