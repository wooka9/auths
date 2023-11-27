-module(auths_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
%	SupFlags = #{
%		strategy => one_for_one,
%		intensity => 1000,
%		period => 1
%	},
	SupFlags = {one_for_one, 5, 10},
	%Auths = ?CHILD(auths, worker),
	Workers = [
		#{
			id => auths,
			start => {auths, start_link, []},
			type => worker,
			shutdown => brutal_kill}
		],
	{ok, {SupFlags, Workers}}.
