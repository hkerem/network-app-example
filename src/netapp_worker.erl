%%%-------------------------------------------------------------------
%%% @author H. Kerem Cevahir <kerem@medratech.com>
%%% @copyright 2009, H. Kerem Cevahir
%%% @doc Worker for netapp.
%%% @end
%%%-------------------------------------------------------------------
-module(netapp_worker).
-author("kerem@medratech.com").
-behaviour(gen_server).

%% API
-export([start_cluster/0, work/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {id=0}).

start_cluster() ->
    gen_server_cluster:start(?MODULE, ?MODULE, [], []).

work(Socket) ->
    gen_server:call({global,?MODULE}, {work, Socket}, infinity).

stop() ->
    gen_server:call({global,?MODULE}, stop).

init([]) ->
    {ok, #state{}}.

handle_call({work, Socket}, _From, State) ->
    Value = State#state.id,
	Reply = business_logic(Socket),
    {reply, Reply, State#state{id=Value+1}};

handle_call(stop, _From, State) ->
    {stop, normalStop, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

business_logic(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            gen_tcp:send(Socket, Data),
            business_logic(Socket);
        {error, closed} ->
            ok
    end.

