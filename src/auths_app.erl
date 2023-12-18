-module(auths_app).

-behaviour(application).

-include("auths_def.hrl").

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    {ok, ConfigWeb} = application:get_env(auths, web),
    Port = proplists:get_value(port, ConfigWeb, ?WEB_PORT_DEF),
    %Url = proplists:get_value(port, ConfigWeb, ?WEB_URL_DEF),
    Routes = [
        {'_', [
            {"/auths/api",      auths_handler, []},
            {"/[...]",          auths_handler, []}
        ]}
    ],
    Dispatch = cowboy_router:compile(Routes),
    {ok, _} = cowboy:start_clear(http, [{port, Port}], #{
        env => #{dispatch => Dispatch}
    }),
    auths_sup:start_link().

stop(_State) ->
    ok.
