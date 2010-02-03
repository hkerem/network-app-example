-module(netapp_worker_sup).

-author('kerem@medratech.com').
-author('saleyn@gmail.com').

-behaviour(supervisor).

%% Application and Supervisor callbacks
-export([init/1]).

-include("netapp.hrl").

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
init([Workers]) ->
	init(Workers, []).

init([Worker|Workers], ChildSpecs) ->
	{Module, _Func, _Args} = Worker,
	init (Workers,
		[
			{
				Module,					% Id	   = internal id
				Worker,					% StartFun = {M, F, A}
				permanent,				% Restart  = permanent | transient | temporary
				?KILL_TIMEOUT,			% Shutdown = brutal_kill | int() >= 0 | infinity
				worker,					% Type	 = worker | supervisor
				[Module]				% Modules  = [Module] | dynamic
			}
		|ChildSpecs]
	);

init([], ChildSpecs) ->
	{ok,
		{
			_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
			lists:reverse(ChildSpecs)
		}
	}.
												 
