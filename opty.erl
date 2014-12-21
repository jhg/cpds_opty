-module(opty).
-export([start/6, stop/1, start_server/1, start_clients/7, stop_sep/2]).

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Updates: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)
%start(Clients, Entries, Updates, Time) ->
%    register(s, server:start(Entries)),
%    L = startClients(Clients, [], Entries, Updates),
%    io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w UPDATES PER TRANSACTION, 
%    DURATION ~w s ~n", [Clients, Entries, Updates, Time]),
%    timer:sleep(Time*1000),
%    stop(L).

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Updates: Number of write operations per transaction
%% Reads: Number of read operations per transaction
%% Time: Duration of the experiment (in secs)
start_server(Entries) ->
  register(s, server:start(Entries)).

start_clients(Clients, Entries, Updates, Reads, Time, ServerNode, NSubset) ->
  L = startClients_sep(Clients, [], Entries, Updates, Reads, ServerNode, NSubset),
  io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w UPDATES PER TRANSACTION, ~w SIZE OF SUBSET OF ENTRIES,
    DURATION ~w s ~n", [Clients, Entries, Updates, NSubset, Time]),
  timer:sleep(Time*1000),
  stop_sep(L, ServerNode).

start(Clients, Entries, Updates, Reads, Time, NSubset) ->
    register(s, server:start(Entries)),
    L = startClients(Clients, [], Entries, Updates, Reads, NSubset),
    io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w UPDATES PER TRANSACTION, ~w SIZE OF SUBSET OF ENTRIES,
    DURATION ~w s ~n", [Clients, Entries, Updates, NSubset, Time]),
    timer:sleep(Time*1000),
    stop(L).

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Updates: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)
%% NSubset: size of subset of entries
%% start(Clients, Entries, Updates, Time, NSubset) ->
%%     register(s, server:start(Entries)),
%%     L = startClients(Clients, [], Entries, Updates, NSubset),
%%     io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w UPDATES PER TRANSACTION, ~w SIZE OF SUBSET OF ENTRIES,
%%     DURATION ~w s ~n", [Clients, Entries, Updates, NSubset, Time]),
%%     timer:sleep(Time*1000),
%%     stop(L).

stop_sep(L, ServerNode) ->
  io:format("Stopping...~n"),
  stopClients(L),
  {s, ServerNode} ! stop.

stop(L) ->
    io:format("Stopping...~n"),
    stopClients(L),
    s ! stop.

startClients_sep(0,L,_,_,_,_,_) -> L;

startClients_sep(Clients, L, Entries, Updates, Reads, ServerNode, NSubset) ->
  Pid = client:start(Clients, Entries, Updates, Reads, {s, ServerNode}, NSubset),
  startClients_sep(Clients-1, [Pid|L], Entries, Updates, Reads, ServerNode, NSubset).

%startClients(0,L,_,_) -> L;
%startClients(Clients, L, Entries, Updates) ->
%    Pid = client:start(Clients, Entries, Updates, s),
%    startClients(Clients-1, [Pid|L], Entries, Updates).

startClients(0,L,_,_,_,_) -> L;
startClients(Clients, L, Entries, Updates, Reads, NSubset) ->
    Pid = client:start(Clients, Entries, Updates, Reads, s, NSubset),
    startClients(Clients-1, [Pid|L], Entries, Updates, Reads, NSubset).

stopClients([]) -> ok;
stopClients([Pid|L]) ->
    Pid ! {stop, self()},
    receive
        {done, Pid} -> ok
    end,
    stopClients(L).
