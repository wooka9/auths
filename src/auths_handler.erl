-module(auths_handler).

%% API
-export([init/2, answer/2]).


init(Req0, _Opts) ->
	Method = cowboy_req:method(Req0),
	HasBody = cowboy_req:has_body(Req0),
	method_check(Method, HasBody, Req0).

method_check(<<"POST">>, true, Req0) ->
	{ok, PostVals, _Req} = cowboy_req:read_urlencoded_body(Req0),
	[{Body, true}] = PostVals,
	Json = jsone:decode(Body),
	Action = maps:get(<<"action">>, Json),
	Data = {Action, Json},
	action(Data, Req0);

method_check(_, _, Req0) ->
	cowboy_req:reply(200, #{
		<<"content-type">>  =>  <<"text/plain">>
		}, <<"Wrong method.\n">>, Req0).

answer(Value, Req0) ->
	cowboy_req:reply(200, #{
		<<"content-type">>  =>  <<"text/plain">>
		}, jsone:encode(#{<<"result" >> => Value}), Req0).

action({<<"useradd">>, Json}, Req0) ->
	User = map_get(<<"username">>, Json),
	Pass = map_get(<<"password">>, Json),
	ok = auths:useradd(User, Pass),
	answer(<<"ok">>, Req0).
