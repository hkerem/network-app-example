-module(netapp_sup).

-author('kerem@medratech.com').
-author('saleyn@gmail.com').

-behaviour(supervisor).

%% Internal API
-export([start_client/1]).

%% Application and Supervisor callbacks
-export([init/1]).

-include("netapp.hrl").

%% A startup function for spawning new client connection handling FSM.
%% To be called by the TCP listener process.
start_client(SocketSup) ->
	supervisor:start_child(SocketSup, []).

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
init([protocol_supervisor, ProtoConf]) ->
	{Proto, {acceptor, {Port, plain, FsmModule}}, _} = ProtoConf,
	AcceptorName = list_to_atom(atom_to_list(Proto) ++ "_acceptor"),
	AcceptorSupName = list_to_atom(atom_to_list(Proto) ++ "_acceptor_sup"),
	WorkerSupName = list_to_atom(atom_to_list(Proto) ++ "_worker_sup"),
	SocketSupName = list_to_atom(atom_to_list(Proto) ++ "_socket_sup"),

	{ok,
		{_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
			[
				% TCP Listener
			  {   AcceptorSupName,								% Id	   = internal id
				  {netapp_acceptor, start_link,
				  	[AcceptorName, Port, SocketSupName, FsmModule]
				  },										% StartFun = {M, F, A}
				  permanent,								% Restart  = permanent | transient | temporary
				  2000,										% Shutdown = brutal_kill | int() >= 0 | infinity
				  worker,									% Type	 = worker | supervisor
				  [netapp_acceptor]							% Modules  = [Module] | dynamic
			  },
				% Echo Worker 
			  {   WorkerSupName,								% Id	   = internal id
				  {netapp_echo_worker,start_cluster,[]},	% StartFun = {M, F, A}
				  permanent,								% Restart  = permanent | transient | temporary
				  2000,										% Shutdown = brutal_kill | int() >= 0 | infinity
				  worker,									% Type	 = worker | supervisor
				  [netapp_echo_worker]							% Modules  = [Module] | dynamic
			  },
				% Client instance supervisor
			  {   SocketSupName,
				  {supervisor,start_link,[{local, SocketSupName}, ?MODULE, [socket, FsmModule]]},
				  permanent,								% Restart  = permanent | transient | temporary
				  infinity,									% Shutdown = brutal_kill | int() >= 0 | infinity
				  supervisor,								% Type	 = worker | supervisor
				  []										% Modules  = [Module] | dynamic
			  }
			]
		}
	};

init([socket, FsmModule]) ->
	{ok,
		{_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
			[
				% TCP Client
			  {   undefined,								% Id	   = internal id
				  {FsmModule,start_link,[]},					% StartFun = {M, F, A}
				  temporary,								% Restart  = permanent | transient | temporary
				  2000,										% Shutdown = brutal_kill | int() >= 0 | infinity
				  worker,									% Type	 = worker | supervisor
				  []										% Modules  = [Module] | dynamic
			  }
			]
		}
	}.

