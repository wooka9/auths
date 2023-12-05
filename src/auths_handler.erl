-module(auths_handler).

-export([init/2]).

-include("auths.hrl").

-define(CALL_TIMEOUT_DEF, 1000).

init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req),
    method_check(Method, HasBody, Req, Opts).

method_check(<<"POST">>, true, Req, Opts) ->
    {ok, Body, _Req} = cowboy_req:read_body(Req),
    Map = jsone:decode(Body, [{object_format, map}]),
    Action = maps:get(<<"action">>, Map, none),
    case is_binary(Action) =/= <<"">> of
            true -> action({Action, Map}, Req, Opts);
            false -> action({none, Map}, Req, Opts)
    end;
method_check(_, _, Req, _Opts) ->
    Headers = #{<<"content-type">> => <<"text/plain">>},
    Body = <<"wrong method">>,
    cowboy_req:reply(200, Headers, [Body, <<"\n">>], Req).

action({<<"useradd">>, Map}, Req, Opts) ->
    Username = maps:get(<<"username">>, Map),
    Password = maps:get(<<"password">>, Map),
    answer({useradd, Username, Password}, Req, Opts);
action({<<"login">>, Map}, Req, Opts) ->
    Username = maps:get(<<"username">>, Map),
    Password = maps:get(<<"password">>, Map),
    answer({login, Username, Password}, Req, Opts);
action({<<"logout">>, Map}, Req, Opts) ->
    Username = maps:get(<<"username">>, Map),
    Session = maps:get(<<"session">>, Map),
    answer({logout, Username, Session}, Req, Opts);
action({<<"ping">>, Map}, Req, Opts) ->
    Username = maps:get(<<"username">>, Map),
    Session = maps:get(<<"session">>, Map),
    answer({ping, Username, Session}, Req, Opts);
action({_, _Map}, Req, Opts) ->
    Result = {error, "not implemented yet"},
    answer_http(Result, Req, Opts).

answer({Action, Arg1, Arg2}, Req, Opts) ->
    PoolName = proplists:get_value(poolname, Opts, ?POOL_NAME_DEF),
    Fun = fun(Worker) -> gen_server:call(Worker, {Action, Arg1, Arg2}, ?CALL_TIMEOUT_DEF) end,
    Result = poolboy:transaction(PoolName, Fun),
    answer_http(Result, Req, Opts).

answer_http(Result, Req, _Opts) ->
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
