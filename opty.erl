-module(opty).
-export([start/5, stop/1]).

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
%start(Clients, Entries, Updates, Reads, Time) ->
%    register(s, server:start(Entries)),
%    L = startClients(Clients, [], Entries, Updates, Reads),
%    io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w UPDATES PER TRANSACTION, ~w READS PER TRANSACTION, 
%    DURATION ~w s ~n", [Clients, Entries, Updates, Reads, Time]),
%    timer:sleep(Time*1000),
%    stop(L).

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Updates: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)
%% NSubset: size of subset of entries
start(Clients, Entries, Updates, Time, NSubset) ->
    register(s, server:start(Entries)),
    L = startClients(Clients, [], Entries, Updates, NSubset),
    io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w UPDATES PER TRANSACTION, ~w SIZE OF SUBSET OF ENTRIES, 
    DURATION ~w s ~n", [Clients, Entries, Updates, NSubset, Time]),
    timer:sleep(Time*1000),
    stop(L).

stop(L) ->
    io:format("Stopping...~n"),
    stopClients(L),
    s ! stop.

%startClients(0,L,_,_) -> L;
%startClients(Clients, L, Entries, Updates) ->
%    Pid = client:start(Clients, Entries, Updates, s),
%    startClients(Clients-1, [Pid|L], Entries, Updates).

startClients(0,L,_,_,_) -> L;
startClients(Clients, L, Entries, Updates, NSubset) ->
    Pid = client:start(Clients, Entries, Updates, s, NSubset),
    startClients(Clients-1, [Pid|L], Entries, Updates, NSubset).

stopClients([]) -> ok;
stopClients([Pid|L]) ->
    Pid ! {stop, self()},
    receive
        {done, Pid} -> ok
    end,
    stopClients(L).
