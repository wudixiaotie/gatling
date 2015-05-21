-module (test).


-compile (export_all).


loop (State) ->
    receive
        Any ->
            io:format("hello, ~p, State:~p~n", [Any, State]),
            loop (State)
    end.