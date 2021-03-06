-module(client).
-export([start/6]).

start(Name, Entries, Updates, Reads, Server, NSubset) ->
    spawn(fun() -> open(Name, Entries, Updates, Reads, Server, NSubset, 0, 0) end).

open(Name, Entries, Updates, Reads, Server, NSubset, Total, Ok) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    Server ! {open, self()},
    receive
        {stop, From} ->
            io:format("~w: Transactions TOTAL:~w, OK:~w, -> ~w % ~n",
            [Name, Total, Ok, 100*Ok/Total]),
            From ! {done, self()},
            ok;
        {transaction, Validator, Store} ->
            Handler = handler:start(self(), Validator, Store),
            %%Defining subset of entries for client
            Subset = choose_random_entries_subset(Entries, NSubset, []),
            do_transactions(Name, Entries, Updates, Reads, Server, Handler, NSubset, Total, Ok, Updates, Reads, Subset)
    end.

% Commit transaction
do_transactions(Name, Entries, Updates, Reads, Server, Handler, NSubset, Total, Ok, 0, 0, _) ->
%%     io:format("~w: Commit: TOTAL ~w, OK ~w~n", [Name, Total, Ok]),
    %timer:sleep(Name*10),
    Ref = make_ref(),
    Handler ! {commit, Ref},
    Result = receiveCommitValue(Ref),
    if
        Result == ok ->
            open(Name, Entries, Updates, Reads, Server, NSubset, Total+1, Ok+1);
        true ->
            open(Name, Entries, Updates, Reads, Server, NSubset, Total+1, Ok)
    end;

% Reads and Writes
do_transactions(Name, Entries, Updates, Reads, Server, Handler, NSubset, Total, Ok, Nwrites, Nreads, Subset) ->
    %io:format("~w: R/W: TOTAL ~w, OK ~w, N ~w~n", [Name, Total, Ok, N]),
    N = random:uniform(),
    if
        N >= 0.5 ->
            do_read(Name, Entries, Updates, Reads, Server, Handler, NSubset, Total, Ok, Nwrites, Nreads, Subset);
        true ->
            do_write(Name, Entries, Updates, Reads, Server, Handler, NSubset, Total, Ok, Nwrites, Nreads, Subset)
    end.

do_read(Name, Entries, Updates, Reads, Server, Handler, NSubset, Total, Ok, Nwrites, Nreads, Subset) ->
    if
        Nreads == 0 ->
            do_transactions(Name, Entries, Updates, Reads, Server, Handler, NSubset, Total, Ok, Nwrites, Nreads, Subset);
        true ->
            Ref = make_ref(),
            Num = choose_random_entry(Subset),
            Handler ! {read, Ref, Num},
            do_transactions(Name, Entries, Updates, Reads, Server, Handler, NSubset, Total, Ok, Nwrites, Nreads-1, Subset)
    end.

do_write(Name, Entries, Updates, Reads, Server, Handler, NSubset, Total, Ok, Nwrites, Nreads, Subset) ->
    if
        Nwrites == 0 ->
            do_transactions(Name, Entries, Updates, Reads, Server, Handler, NSubset, Total, Ok, Nwrites, Nreads, Subset);
        true ->
            Num = choose_random_entry(Subset),
            if
              Nreads == 0 ->
                RandomValue = random:uniform(100),
                Handler ! {write, Num, RandomValue},
                do_transactions(Name, Entries, Updates, Reads, Server, Handler, NSubset, Total, Ok, Nwrites-1, Nreads, Subset);
              true ->
                Ref = make_ref(),
                Handler ! {read, Ref, Num},
                Value = receiveValue(Ref),
                Handler ! {write, Num, Value+1},
                do_transactions(Name, Entries, Updates, Reads, Server, Handler, NSubset, Total, Ok, Nwrites-1, Nreads-1, Subset)
            end
    end.


choose_random_entries_subset(_, 0, Subset) ->
  Subset;

choose_random_entries_subset(Entries, Size, Subset) ->
  Entry = random:uniform(Entries),
  case lists:member(Entry, Subset) of
    true ->
      choose_random_entries_subset(Entries, Size, Subset);
    false ->
      choose_random_entries_subset(Entries, Size-1, [Entry|Subset])
  end.

choose_random_entry(Subset) ->
  SubsetLength = length(Subset),
  Num = random:uniform(SubsetLength),
  get_entry_at_pos(Num, Subset, 1).

get_entry_at_pos(Pos, [H|T], CurrentPos) ->
  if
    Pos == CurrentPos ->
      H;
    true ->
      get_entry_at_pos(Pos, T, CurrentPos+1)
  end.

receiveCommitValue(Ref) ->
    receive
        {Ref,Value} -> Value
    end.

receiveValue(Ref) ->
    receive
        {value,Ref,Value} -> Value
    end.
