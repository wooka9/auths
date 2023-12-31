-module(auths_handler).

-export([init/2]).

-include("auths_def.hrl").

-define(CALL_TIMEOUT_DEF, 1000).

init(Req, _Opts) ->
    Method = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req),
    method_check(Method, HasBody, Req).

method_check(<<"POST">>, true, Req) ->
    {ok, Body, _Req} = cowboy_req:read_body(Req),
    Map = jsone:decode(Body, [{object_format, map}]),
    Action = maps:get(<<"action">>, Map, none),
    case is_binary(Action) andalso Action =/= <<"">> of
            true ->
                action({Action, Map}, Req);
            false ->
                action({none, Map}, Req)
    end;
method_check(_, _, Req) ->
    Headers = #{<<"content-type">> => <<"text/plain">>},
    Body = <<"wrong method">>,
    cowboy_req:reply(200, Headers, [Body, <<"\n">>], Req).

action({<<"useradd">>, Map}, Req) ->
    Username = maps:get(<<"username">>, Map),
    Password = maps:get(<<"password">>, Map),
    answer({useradd, [Username, Password]}, Req);
action({<<"login">>, Map}, Req) ->
    Username = maps:get(<<"username">>, Map),
    Password = maps:get(<<"password">>, Map),
    answer({login, [Username, Password]}, Req);
action({<<"logout">>, Map}, Req) ->
    Username = maps:get(<<"username">>, Map),
    Session = maps:get(<<"session">>, Map),
    answer({logout, [Username, Session]}, Req);
action({<<"ping">>, Map}, Req) ->
    Username = maps:get(<<"username">>, Map),
    Session = maps:get(<<"session">>, Map),
    answer({ping, [Username, Session]}, Req);
action({<<"close">>, _Map}, Req) ->
    answer({close, [0,1]}, Req);
action({_, _Map}, Req) ->
    Result = {error, "not implemented"},
    answer_http(Result, Req).

answer({Action, Args}, Req) ->
    {ok, ConfigPool} = application:get_env(auths, pool),
    PoolName = proplists:get_value(name, ConfigPool, ?POOL_NAME_DEF),
    Fun = fun(Worker) -> gen_server:call(Worker, {Action, Args}, ?CALL_TIMEOUT_DEF) end,
    Result = poolboy:transaction(PoolName, Fun),
    answer_http(Result, Req).

answer_http(Result, Req) ->
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
