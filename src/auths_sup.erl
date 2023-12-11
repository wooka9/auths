-module(auths_sup).

-behaviour(supervisor).

-include("auths_def.hrl").

-export([start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%init(WorkerArgs) ->
init([]) ->
    {ok, Pool} = application:get_env(auths, pool),
    PoolName = proplists:get_value(name, Pool, ?POOL_NAME_DEF),
    PoolSize = proplists:get_value(size, Pool, ?POOL_SIZE_DEF),
    SizeArgs = [
        {size, PoolSize},
        {max_overflow, PoolSize}
    ],
    PoolArgs = [
        {name, {local, PoolName}},
        {worker_module, auths_worker}] ++ SizeArgs,
    {ok, DB} = application:get_env(auths, db),
    WorkerArgs = [{db, maps:from_list(DB)}],
    PoolSpecs = poolboy:child_spec(PoolName, PoolArgs, WorkerArgs),
    {ok, {{one_for_one, 10, 10}, [PoolSpecs]}}.
