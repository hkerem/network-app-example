-module(netapp).

-author('kerem@medratech.com').

-export([start/0]).

start() ->
	ssl:start(),
	application:start(netapp).

