-module(opty_sep).
-export([start_clients/5, start_server/2, stop/1]).

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Updates: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)
start_clients(Clients, Entries, Updates, Time, ServerNode) ->
    L = startClients(Clients, [], Entries, Updates, ServerNode),
    io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w UPDATES PER TRANSACTION, 
    DURATION ~w s ~n", [Clients, Entries, Updates, Time]),
    timer:sleep(Time*1000),
    stop(L).

start_server(Entries, ClientNode) ->
    server_sep:start(Entries, ClientNode).

stop(L) ->
    io:format("Stopping...~n"),
    stopClients(L),
    s ! stop.

startClients(0,L,_,_,_) -> L;
startClients(Clients, L, Entries, Updates, ServerNode) ->
    Pid = client_sep:start(Clients, Entries, Updates, {s, ServerNode}),
    startClients(Clients-1, [Pid|L], Entries, Updates, ServerNode).

stopClients([]) -> ok;
stopClients([Pid|L]) ->
    Pid ! {stop, self()},
    receive
        {done, Pid} -> ok
    end,
    stopClients(L).
