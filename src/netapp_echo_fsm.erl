-module(netapp_echo_fsm).

-author('kerem@medratech.com').

-export([init/0]).

%% FSM States
-export([
	'WAIT_FOR_LINE'/2
]).

-record(state, {count}).

%% determining initial FSM state and creating state record.
init() ->
	{ok, 'WAIT_FOR_LINE', #state{count=0}}.

'WAIT_FOR_LINE'({data, Data}, State) ->
	%% This request will be processed by clustered workers. 
	%% ReplyData = netapp_echo_worker:echo_reply(Data),
	ReplyData = netapp_echo_worker:async_echo_reply(Data),
	Count=State#state.count,
	io:format("fsm count id=~w~n", [Count]),
	{next_state, 'WAIT_FOR_LINE', State#state{count=Count+1}, ReplyData}.
