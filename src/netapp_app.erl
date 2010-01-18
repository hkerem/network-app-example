-module(netapp_app).

-author('kerem@medratech.com').
-author('saleyn@gmail.com').

-behaviour(application).

%% Internal API
-export([start_client/0]).

%% Application and Supervisor callbacks
-export([start/2, stop/1, init/1]).

-define(MAX_RESTART,	5).
-define(MAX_TIME,	  60).
-define(DEF_PORT,	2222).

%% A startup function for spawning new client connection handling FSM.
%% To be called by the TCP listener process.
start_client() ->
	supervisor:start_child(netapp_socket_sup, []).

%%----------------------------------------------------------------------
%% Application behaviour callbacks
%%----------------------------------------------------------------------
start(_Type, _Args) ->
	ListenPort = get_app_env(listen_port, ?DEF_PORT),
	supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenPort, netapp_echo_fsm]).

stop(_S) ->
	ok.

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
init([Port, Module]) ->
	{ok,
		{_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
			[
				% TCP Listener
			  {   netapp_acceptor_sup,							% Id	   = internal id
				  {netapp_acceptor,start_link,[Port,Module]},	% StartFun = {M, F, A}
				  permanent,								% Restart  = permanent | transient | temporary
				  2000,										% Shutdown = brutal_kill | int() >= 0 | infinity
				  worker,									% Type	 = worker | supervisor
				  [netapp_acceptor]							% Modules  = [Module] | dynamic
			  },
				% Echo Worker 
			  {   netapp_echo_worker_sup,							% Id	   = internal id
				  {netapp_echo_worker,start_cluster,[]},	% StartFun = {M, F, A}
				  permanent,								% Restart  = permanent | transient | temporary
				  2000,										% Shutdown = brutal_kill | int() >= 0 | infinity
				  worker,									% Type	 = worker | supervisor
				  [netapp_echo_worker]							% Modules  = [Module] | dynamic
			  },
				% Client instance supervisor
			  {   netapp_socket_sup,
				  {supervisor,start_link,[{local, netapp_socket_sup}, ?MODULE, [Module]]},
				  permanent,								% Restart  = permanent | transient | temporary
				  infinity,									% Shutdown = brutal_kill | int() >= 0 | infinity
				  supervisor,								% Type	 = worker | supervisor
				  []										% Modules  = [Module] | dynamic
			  }
			]
		}
	};

init([Module]) ->
	{ok,
		{_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
			[
				% TCP Client
			  {   undefined,								% Id	   = internal id
				  {Module,start_link,[]},					% StartFun = {M, F, A}
				  temporary,								% Restart  = permanent | transient | temporary
				  2000,										% Shutdown = brutal_kill | int() >= 0 | infinity
				  worker,									% Type	 = worker | supervisor
				  []										% Modules  = [Module] | dynamic
			  }
			]
		}
	}.

%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------
get_app_env(Opt, Default) ->
	case application:get_env(application:get_application(), Opt) of
	{ok, Val} -> Val;
	_ ->
		case init:get_argument(Opt) of
		[[Val | _]] -> Val;
		error	   -> Default
		end
	end.
