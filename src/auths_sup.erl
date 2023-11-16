-module(auths_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
		#{
			id => auths_server,
			start => {auths, start_link, []},
			type => worker,
			shutdown => brutal_kill}
		],
		%%{auth, {auth, start_link, []},
		%%permanent, 5000, worker, [auth]}],
	{ok, {{one_for_one, 5, 10}, Procs}}.
