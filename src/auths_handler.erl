-module(auths_handler).

-export([init/2, answer/2]).


init(Req, _Opts) ->
	Method = cowboy_req:method(Req),
	HasBody = cowboy_req:has_body(Req),
	method_check(Method, HasBody, Req).

method_check(<<"POST">>, true, Req) ->
	{ok, Body, _Req} = cowboy_req:read_body(Req),
	Map = jsone:decode(Body, [{object_format, map}]),
	Action = maps:get(<<"action">>, Map),
	action({Action, Map}, Req);

method_check(_, _, Req) ->
	Headers = #{<<"content-type">> => <<"text/plain">>},
	Body = <<"wrong method">>,
	cowboy_req:reply(200, Headers, [Body, <<"\n">>], Req).

action({<<"useradd">>, Map}, Req) ->
	Username = maps:get(<<"username">>, Map),
	Password = maps:get(<<"password">>, Map),
	Result = auths:useradd(Username, Password),
	answer(Result, Req);
action({<<"login">>, Map}, Req) ->
	Username = maps:get(<<"username">>, Map),
	Password = maps:get(<<"password">>, Map),
	Result = auths:login(Username, Password),
	answer(Result, Req);
action({<<"logout">>, Map}, Req) ->
	Username = maps:get(<<"username">>, Map),
	Session = maps:get(<<"session">>, Map),
	Result = auths:logout(Username, Session),
	answer(Result, Req);
action({<<"ping">>, Map}, Req) ->
	Username = maps:get(<<"username">>, Map),
	Session = maps:get(<<"session">>, Map),
	Result = auths:ping(Username, Session),
	answer(Result, Req);
action({_, _Map}, Req) ->
	Result = {error, <<"not implemented yet">>},
	answer(Result, Req).

answer(Result, Req) ->
	Headers = #{<<"content-type">> => <<"application/json">>},
	Body = jsone:encode(result_to_map(Result)),
	cowboy_req:reply(200, Headers, [Body, <<"\n">>], Req).

result_to_map(Result) ->
	case Result of
		{ok, Message} ->
			#{<<"status">> => <<"ok">>, <<"message">> => list_to_binary(Message)};
		{ok, Message, #{<<"session">> := Session}} ->
			#{<<"status">> => <<"ok">>, <<"message">> => list_to_binary(Message), <<"session">> => Session};
		{error, Message} ->
			#{<<"status">> => <<"error">>, <<"message">> => list_to_binary(Message)};
		{_} ->
			#{<<"status">> => <<"abnormal">>}
	end.
