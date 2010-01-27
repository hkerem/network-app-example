%%%-------------------------------------------------------------------
%%% @author H. Kerem Cevahir <kerem@medratech.com>
%%% @copyright 2009, H. Kerem Cevahir
%%% @doc Worker for netapp.
%%% @end
%%%-------------------------------------------------------------------
-module(netapp_echo_worker).
-author("kerem@medratech.com").
-behaviour(gen_server).

%% API
-export([start_cluster/0, echo_reply/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {id=0}).

start_cluster() ->
    gen_server_cluster:start(?MODULE, ?MODULE, [], []).

echo_reply(Request) ->
    gen_server:call({global,?MODULE}, {echo_reply, Request}).

stop() ->
    gen_server:call({global,?MODULE}, stop).

init([]) ->
    {ok, #state{}}.

handle_call({echo_reply, Request}, _From, State) ->
	Value = State#state.id,
	Reply = Request,
	io:format("echo replied by ~s...~n", [node()]),
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

