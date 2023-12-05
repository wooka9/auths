-module(auths_app).

-behaviour(application).

-include("auths.hrl").

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Url = application:get_env(web, url, ?WEB_URL_DEF),
    Port = application:get_env(web, port, ?WEB_PORT_DEF),
    PoolName = application:get_env(pool, name, ?POOL_NAME_DEF),
    Dispatch = cowboy_router:compile([
        {'_', [
            {Url, auths_handler, [{poolname, PoolName}]}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, Port}], #{
        env => #{dispatch => Dispatch}
    }),
    auths_sup:start_link().

stop(_State) ->
    ok.
