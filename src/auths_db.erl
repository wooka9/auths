-module(auths_db).
-export([create/1, useradd/3, login/4, logout/3, ping/3, cleanup/1]).

create(DB) ->
	% case ets:whereis(DB)
	ets:new(DB, [named_table, public]),
	ok.

useradd(DB, Username, Password) ->
	case is_integer(Username) or is_integer(Password) of
		false ->
			{ok, C} = epgsql:connect(DB),
			{ok, Count} = epgsql:equery(C, "insert into users (username, password) values ($1, $2)", [Username, Password]),
			io:format("~nCount: ~p~n", [Count]),
			ok = epgsql:close(C),
			{ok, "username and password added to DB"};
		true ->
			{error, "username or password format invalid"}
	end.

login(DB, Username, Password, _ExpireIn) ->
	io:format("~nUsername: ~p, Password: ~p~n", [Username, Password]),
	{ok, C} = epgsql:connect(DB),
	{ok, Columns, Rows} = epgsql:equery(C, "select count(*) from users where username = $1 and password = $2", [Username, Password]),
	io:format("~nColumns: ~p, Rows: ~p~n", [Columns, Rows]),
	case Rows of
		[{1}] ->
			Session = erlang:phash2({node(), erlang:system_time()}),
			%{ok, _Count} = epgsql:equery(C, "insert into sessions (username, session, expireat) values ($1, $2, NOW() + INTERVAL '$3 seconds')", [Session, Username, ExpireIn]),
			{ok, _Count} = epgsql:equery(C, "insert into sessions (username, session, expireat) values ($1, $2, NOW())", [Session, Username]),
			ok = epgsql:close(C),
			{ok, "session added", #{<<"session">> => Session}};
		_ ->
			ok = epgsql:close(C),
			{error, "username or password are incorrect"}
	end.

logout(DB, Username, Session) ->
	io:format("~nUsername: ~p, Session: ~p~n", [Username, Session]),
	{ok, C} = epgsql:connect(DB),
	{ok, Count} = epgsql:equery(C, "delete from sessions where username = $1 and session = $2", [Username, Session]),
	io:format("~nCount: ~p~n", [Count]),
	ok = epgsql:close(C),
	case Count of
		1 ->
			{ok, "sessions closed"};
		_ ->
			{error, "session does not exists"}
	end.

ping(DB_sessions, Username, Session) ->
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
