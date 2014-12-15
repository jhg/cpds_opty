-module(entry).
-export([new/1]).

new(Value) ->
    spawn_link(fun() -> init(Value) end).

init(Value) ->
    entry(Value, make_ref()).

entry(Value, Time) ->
    receive
        {read, Ref, From} ->
            io:format("[Reference ~w]: ~w from ~w at time ~w~n", [Ref, Value, From, Time]),
            entry(Value, Time);
        {write, New} ->
            entry(New, make_ref());  %% TODO: COMPLETE
        {check, Ref, Readtime, From} ->
            if 
                Readtime == Ref ->   %% TODO: COMPLETE
                    io:format("[Reference ~w] has the same timestamp~n");
                true -> %% from Victoria shouldnt this be false???
                    From ! {Ref, abort}
            end,
            entry(Value, Time);
        stop ->
            ok
    end.