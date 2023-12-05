-module(auths_sup).

-behaviour(supervisor).

-include("auths.hrl").

-export([start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%init(WorkerArgs) ->
init([]) ->
    PoolName = application:get_env(pool, name, ?POOL_NAME_DEF),
    PoolSize = application:get_env(pool, size, ?POOL_SIZE_DEF),
    SizeArgs = [
        {size, PoolSize},
        {max_overflow, PoolSize}
    ],
    PoolArgs = [
        {name, {local, PoolName}},
        {worker_module, auths_worker}] ++ SizeArgs,
    Hostname = application:get_env(db, hostname, "localhost"),
    Database = application:get_env(db, database, "auths"),
    Username = application:get_env(db, username, "auths"),
    Password = application:get_env(db, password, "auths"),
    DB = #{host => Hostname, database => Database, username => Username, password => Password, timeout => 4000},
    WorkerArgs = [{db, DB}],
    PoolSpecs = poolboy:child_spec(PoolName, PoolArgs, WorkerArgs),
    {ok, {{one_for_one, 10, 10}, [PoolSpecs]}}.
