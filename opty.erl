-module(opty).
-export([start/5, stop/1, start_server/1, start_clients/6, stop_sep/2]).

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
%% Updates: Number of read operations per transaction
%% Time: Duration of the experiment (in secs)
start_server(Entries) ->
  register(s, server:start(Entries)).

start_clients(Clients, Entries, Updates, Reads, Time, ServerNode) ->
  register(s, server:start(Entries)),
  L = startClients_sep(Clients, [], Entries, Updates, Reads, ServerNode),
  io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w UPDATES PER TRANSACTION, ~w READS PER TRANSACTION,
    DURATION ~w s ~n", [Clients, Entries, Updates, Reads, Time]),
  timer:sleep(Time*1000),
  stop_sep(L, ServerNode).

start(Clients, Entries, Updates, Reads, Time) ->
    register(s, server:start(Entries)),
    L = startClients(Clients, [], Entries, Updates, Reads),
    io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w UPDATES PER TRANSACTION, ~w READS PER TRANSACTION, 
    DURATION ~w s ~n", [Clients, Entries, Updates, Reads, Time]),
    timer:sleep(Time*1000),
    stop(L).

stop_sep(L, ServerNode) ->
  io:format("Stopping...~n"),
  stopClients(L),
  {s, ServerNode} ! stop.

stop(L) ->
    io:format("Stopping...~n"),
    stopClients(L),
    s ! stop.

startClients_sep(0,L,_,_,_,_) -> L;
startClients_sep(Clients, L, Entries, Updates, Reads, ServerNode) ->
  %Pid = client:start(Clients, Entries, Updates, s),
  Pid = client:start(Clients, Entries, Updates, Reads, {s, ServerNode}),
  startClients_sep(Clients-1, [Pid|L], Entries, Updates, Reads, ServerNode).

startClients(0,L,_,_,_) -> L;
startClients(Clients, L, Entries, Updates, Reads) ->
    %Pid = client:start(Clients, Entries, Updates, s),
    Pid = client:start(Clients, Entries, Updates, Reads, s),
    startClients(Clients-1, [Pid|L], Entries, Updates, Reads).

stopClients([]) -> ok;
stopClients([Pid|L]) ->
    Pid ! {stop, self()},
    receive
        {done, Pid} -> ok
    end,
    stopClients(L).
