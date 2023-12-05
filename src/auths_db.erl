-module(auths_db).
-export([create/1, useradd/3, login/4, logout/3, ping/3, cleanup/1]).

create(_Conn) ->
	% create tables if doesn't exists
	ok.

useradd(Conn, Username, Password) ->
	case is_integer(Username) or is_integer(Password) of
		false ->
			Stmt = "insert into users (username, password) values ($1, $2)",
			{ok, Count} = epgsql:equery(Conn, Stmt, [Username, Password]),
			io:format("~nCount: ~p~n", [Count]),
			{ok, "username and password added to DB"};
		true ->
			{error, "username or password format invalid"}
	end.

login(Conn, Username, Password, _ExpireIn) ->
	io:format("~nUsername: ~p, Password: ~p~n", [Username, Password]),
	Stmt_select = "select count(*) from users where username = $1 and password = $2",
	{ok, Columns, Rows} = epgsql:equery(Conn, Stmt_select, [Username, Password]),
	io:format("~nColumns: ~p, Rows: ~p~n", [Columns, Rows]),
	case Rows of
		[{1}] ->
			Session = erlang:phash2({node(), erlang:system_time()}),
			Stmt_insert = "insert into sessions (username, session, expireat) values ($1, $2, NOW())",
			%{ok, _Count} = epgsql:equery(Conn, "insert into sessions (username, session, expireat) values ($1, $2, NOW() + INTERVAL '$3 seconds')", [Session, Username, ExpireIn]),
			{ok, _Count} = epgsql:equery(Conn, Stmt_insert, [Username, Session]),
			{ok, "session added", #{<<"session">> => Session}};
		_ ->
			{error, "username or password are incorrect"}
	end.

logout(Conn, Username, Session) ->
	io:format("~nUsername: ~p, Session: ~p~n", [Username, Session]),
	Stmt = "delete from sessions where username = $1 and session = $2",
	{ok, Count} = epgsql:equery(Conn, Stmt, [Username, Session]),
	io:format("~nCount: ~p~n", [Count]),
	case Count of
		1 ->
			{ok, "sessions closed"};
		_ ->
			{error, "session does not exists"}
	end.

ping(Conn, Username, Session) ->
	io:format("~nUsername: ~p, Session: ~p~n", [Username, Session]),
	Stmt = "select from sessions where username = $1 and session = $2",
	{ok, Count} = epgsql:equery(Conn, Stmt, [Username, Session]),
	io:format("~nCount: ~p~n", [Count]),
	case Count of
		1 ->
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
