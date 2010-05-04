-module(netapp_fsm).

-author('kerem@medratech.com').
-author('saleyn@gmail.com').

-behaviour(gen_fsm).

-export([behaviour_info/1]).

-export([start_link/1,
	set_socket/3]).

%% gen_fsm callbacks
-export([init/1,
	handle_event/3,
	handle_sync_event/4,
	handle_info/3,
	terminate/3,
	code_change/4]).

%% FSM States
-export([
	'WAIT_FOR_SOCKET'/2,
	'WAIT_FOR_DATA'/2
]).

-record(state, {
				module, % name of FSM module
				module_state, % state obj for FSM module
				module_fsm_state, % FSM state of FSM module
				socket,	% client socket
				comm_type, % whether socket uses ssl
				addr	   % client address
			   }).

-define(TIMEOUT, 120000).

behaviour_info(callbacks) ->
	[{init, 0}];

behaviour_info(_Other) ->
	undefined.

%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @spec (TargetFsmModule) -> {ok,Pid} | ignore | {error,Error}
%% @doc To be called by the supervisor in order to start the server.
%%	  If init/1 fails with Reason, the function returns {error,Reason}.
%%	  If init/1 returns {stop,Reason} or ignore, the process is
%%	  terminated and the function returns {error,Reason} or ignore,
%%	  respectively.
%% @end
%%-------------------------------------------------------------------------
start_link(TargetModule) ->
	gen_fsm:start_link(?MODULE, [TargetModule], []).

set_socket(Pid, Socket, CommType) when is_pid(Pid) ->
	gen_fsm:send_event(Pid, {socket_ready, Socket, CommType}).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}		  |
%%		  {ok, StateName, StateData, Timeout} |
%%		  ignore							  |
%%		  {stop, StopReason}
%% @private
%%-------------------------------------------------------------------------
init([TargetModule]) ->
	process_flag(trap_exit, true),
	{ok, FirstState, StateObj} = TargetModule:init(),
	{ok, 'WAIT_FOR_SOCKET', #state{
			module=TargetModule, 
			module_fsm_state=FirstState, 
			module_state=StateObj}
		}.

%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}		  |
%%		  {next_state, NextStateName, NextStateData, Timeout} |
%%		  {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
'WAIT_FOR_SOCKET'({socket_ready, Socket, CommType}, State) ->
	% Now we own the socket
	Backend = case CommType of
			plain -> inet;
			ssl -> ssl
		end,
	Backend:setopts(Socket, [{active, once}, {packet, 0}, list]),
	{ok, {IP, _Port}} = Backend:peername(Socket),
	{next_state, 'WAIT_FOR_DATA', 
			State#state{socket=Socket, comm_type=CommType, addr=IP}, 
		?TIMEOUT};

'WAIT_FOR_SOCKET'(Other, State) ->
	error_logger:error_msg("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
	%% Allow to receive async messages
	{next_state, 'WAIT_FOR_SOCKET', State}.

%% Notification event coming from client
'WAIT_FOR_DATA'({data, Data}, State) ->
	Module = State#state.module,
	ModuleFSMState = State#state.module_fsm_state,
	ModuleState = State#state.module_state,

	ModuleReply = Module:ModuleFSMState({data,Data}, ModuleState),
	%% should refactor for other return options.
	{next_state, ModuleFSMState1, ModuleState1, ReplyData} = ModuleReply,

	ok = send(State, ReplyData),

	{next_state, 'WAIT_FOR_DATA', 
			State#state{
				module_state=ModuleState1, 
				module_fsm_state=ModuleFSMState1}, 
			?TIMEOUT};

'WAIT_FOR_DATA'(timeout, State) ->
	error_logger:error_msg("~p Client connection timeout - closing.\n", [self()]),
	{stop, normal, State};

'WAIT_FOR_DATA'(Data, State) ->
	io:format("~p Ignoring data: ~p\n", [self(), Data]),
	{next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}.

%%-------------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}		  |
%%		  {next_state, NextStateName, NextStateData, Timeout} |
%%		  {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
	{stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}			|
%%		  {next_state, NextStateName, NextStateData, Timeout}   |
%%		  {reply, Reply, NextStateName, NextStateData}		  |
%%		  {reply, Reply, NextStateName, NextStateData, Timeout} |
%%		  {stop, Reason, NewStateData}						  |
%%		  {stop, Reason, Reply, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, StateData) ->
	{stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}		  |
%%		  {next_state, NextStateName, NextStateData, Timeout} |
%%		  {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, StateName, 
		#state{socket=Socket, comm_type=plain} = StateData) ->
	% Flow control: enable forwarding of next TCP message
	inet:setopts(Socket, [{active, once}]),
	?MODULE:StateName({data, Bin}, StateData);

handle_info({ssl, Socket, Bin}, StateName, 
		#state{socket=Socket, comm_type=ssl} = StateData) ->
	% Flow control: enable forwarding of next TCP message
	ssl:setopts(Socket, [{active, once}]),
	?MODULE:StateName({data, Bin}, StateData);

handle_info({tcp_closed, Socket}, _StateName,
			#state{socket=Socket, comm_type=plain, addr=Addr} = StateData) ->
	error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Addr]),
	{stop, normal, StateData};

handle_info({ssl_closed, Socket}, _StateName,
			#state{socket=Socket, comm_type=ssl, addr=Addr} = StateData) ->
	error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Addr]),
	{stop, normal, StateData};

handle_info(_Info, StateName, StateData) ->
	{noreply, StateName, StateData}.

%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, #state{socket=Socket}) ->
	(catch gen_tcp:close(Socket)),
	ok.

%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%%%% Internal functions

send(State, Data) ->
	Backend = case State#state.comm_type of
			plain -> gen_tcp;
			ssl -> ssl
		end,
	Backend:send(State#state.socket, Data).
