-module(netapp_app).

-author('kerem@medratech.com').

-behaviour(application).

-export([start/2, init/1, stop/1]).

-include("netapp.hrl").

%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
start(_Type, _Args) ->
	{ok, Protocols} = application:get_env(protocols),
	supervisor:start_link({local, ?MODULE}, ?MODULE, [Protocols]).

init([Protocols]) ->
	init(Protocols, []).

init([ProtoConf| Protocols], ChildSpecs) ->
	{Proto, _, _} = ProtoConf,
	SupName = list_to_atom(atom_to_list(Proto) ++ "_sup"),

	init(Protocols, 
		[
			{	SupName,								% Id	   = internal id
				{supervisor, start_link, 
					[{local, SupName}, netapp_sup, [protocol_supervisor, ProtoConf]]
				},										% StartFun = {M, F, A}
				permanent,								% Restart  = permanent | transient | temporary
				infinity,								% Shutdown = brutal_kill | int() >= 0 | infinity
				supervisor,								% Type	 = worker | supervisor
				[netapp_sup]							% Modules  = [Module] | dynamic
			}
		|ChildSpecs]
	);

init([], ChildSpecs) ->
	Ret = {ok,
		{
			_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
			ChildSpecs
		}
	},
	erlang:display(Ret),
	Ret.



%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored.
%%--------------------------------------------------------------------
stop(_S) ->
	ok.

