-module(auths_db).
-export([create/1, useradd/3, login/4, logout/3]).

create(DB) ->
	ets:new(DB, [named_table, public]),
	ok.

useradd(DB, Username, Password) ->
	ets:insert(DB, {Username, Password}),
	ok.

login(DB, Username, Password, Expire) ->
	case ets:lookup(DB, Username) of
		[{Username, Password}] ->
			TimeNow = erlang:system_time(seconds),
			SessionNew = erlang:phash2({node(), erlang:system_time()}),
			ets:insert(DB, {Username, SessionNew, TimeNow + Expire}),
			{ok, SessionNew};
		_ ->
			{error, "Incorrect Username or Password"}
	end.

logout(DB, UsernameOut, SessionOut) ->
	MatchSpec = ets:fun2ms(fun({Username, Session, _ExpireTime}) when Username == UsernameOut, Session == SessionOut -> true end),
	ets:select_delete(DB, MatchSpec),
	ok.

%delete_obsolete(TableName) ->
%	TimeNow = erlang:system_time(seconds),
%	MatchSpec = ets:fun2ms(fun({_, _, ExpireTime}) when ExpireTime < TimeNow -> true end),
%	ets:select_delete(TableName, MatchSpec),
%	ok.

