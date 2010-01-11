%%%-------------------------------------------------------------------
%%% @author H. Kerem Cevahir <kerem@medratech.com>
%%% @copyright 2009, H. Kerem Cevahir
%%% @doc Acceptor component for netapp.
%%% @end
%%%-------------------------------------------------------------------
-module(netapp_acceptor).
-author("kerem@medratech.com").
-behaviour(gen_server).

%% API
-export([start_link/0, state/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([accept_func/2]).

-include_lib("eunit/include/eunit.hrl").
-include("netapp.hrl").

-define(SERVER, ?MODULE). 

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-record(state, {ip=any, lsocket=null}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server.
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, #state{}, []).

%%--------------------------------------------------------------------
%% @doc Returns server state.
%% @spec state() -> {ok, State}
%% @end
%%--------------------------------------------------------------------
state() ->
	gen_server:call(?SERVER, state).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Initializes the server.
%% @spec init(Args) -> {ok, State} |
%%					 {ok, State, Timeout} |
%%					 ignore |
%%					 {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(State) ->
	case gen_tcp:listen(9090, ?TCP_OPTIONS) of
		{ok, LSocket} ->
			NewState = State#state{lsocket = LSocket},
			{ok, accept(NewState)};
		{error, Reason} ->
			{stop, Reason}
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles call messages.
%% @spec handle_call(Request, From, State) ->
%%				  {reply, Reply, State} |
%%				  {reply, Reply, State, Timeout} |
%%				  {noreply, State} |
%%				  {noreply, State, Timeout} |
%%				  {stop, Reason, Reply, State} |
%%				  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(state, _From, State) ->
	Reply = State,
	{reply, Reply, State};
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles cast messages.
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%								  {noreply, State, Timeout} |
%%								  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({accepted, _Pid}, State) ->
	{noreply, accept(State)};

handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles all non call/cast messages.
%% @spec handle_info(Info, State) -> {noreply, State} |
%%								   {noreply, State, Timeout} |
%%								   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Converts process state when code is changed.
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

accept(State = #state{lsocket = LSocket}) ->
	proc_lib:spawn(?MODULE, accept_func, [self(), LSocket]),
	io:format("Socket is waiting for a new connection to accept... ~n"),
	State.

accept_func(Server, LSocket) ->
	{ok, Socket} = gen_tcp:accept(LSocket),
	gen_server:cast(Server, {accepted, self()}),
	io:format("New connection is accepted, running business routines... ~n"),

	netapp_worker:work(Socket).

