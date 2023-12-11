-module(auths_db).
-export([create/1, useradd/2, login/2, logout/2, ping/2, close/2, cleanup/1]).

create(_Connection) ->
    % create tables if doesn't exists
    ok.

useradd(Connection, [Username, Password]) ->
    case is_integer(Username) or is_integer(Password) of
        false ->
            Stmt = "insert into users (username, password) values ($1, $2)",
            {ok, _Count} = epgsql:equery(Connection, Stmt, [Username, Password]),
            %io:format("~nCount: ~p~n", [Count]),
            {ok, "username and password added to DB"};
        true ->
            {error, "username or password format invalid"}
    end.

login(Connection, Args) ->
    ExpireIn = 60,
    login(Connection, Args, ExpireIn).

login(Connection, [Username, _Password] = Args, _ExpireIn) ->
    %io:format("~nUsername: ~p, Password: ~p~n", [Username, Password]),
    Stmt_select = "select count(*) from users where username = $1 and password = $2",
    {ok, _Columns, Rows} = epgsql:equery(Connection, Stmt_select, Args),
    %io:format("~nColumns: ~p, Rows: ~p~n", [Columns, Rows]),
    case Rows of
        [{1}] ->
            Session = erlang:phash2({node(), erlang:system_time()}),
            Stmt_insert = "insert into sessions (username, session, expireat) values ($1, $2, NOW())",
            %{ok, _Count} = epgsql:equery(Connection, "insert into sessions (username, session, expireat) values ($1, $2, NOW() + INTERVAL '$3 seconds')", [Session, Username, ExpireIn]),
            {ok, _Count} = epgsql:equery(Connection, Stmt_insert, [Username, Session]),
            {ok, "session added", #{<<"session">> => Session}};
        _ ->
            {error, "username or password are incorrect"}
    end.

logout(Connection, Args) ->
    %io:format("~nUsername: ~p, Session: ~p~n", Args),
    Stmt = "delete from sessions where username = $1 and session = $2",
    {ok, Count} = epgsql:equery(Connection, Stmt, Args),
    case Count of
        1 ->
            {ok, "sessions closed"};
        _ ->
            {error, "session does not exists"}
    end.

ping(Connection, Args) ->
    %io:format("~nUsername: ~p, Session: ~p~n", Args),
    Stmt = "select from sessions where username = $1 and session = $2",
    {ok, Count} = epgsql:equery(Connection, Stmt, Args),
    case Count of
        1 ->
            {ok, "pong"};
        _ ->
            {error, "session does not exists"}
    end.

close(Connection, _Args) ->
    ok = epgsql:close(Connection),
    {ok, "connection to db closed"}.

cleanup(_DB) ->
    %TimeNow = erlang:system_time(seconds),
    %MatchSpec = ets:fun2ms(fun({_, _, ExpireTime}) when ExpireTime < TimeNow -> true end),
    %ets:select_delete(DB_sessions, MatchSpec),
    {ok, "cleanup seccessful"}.
