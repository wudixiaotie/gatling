-module (test).

-compile (export_all).

times(State) ->
    receive
        add ->
            State + 1;
        none ->
            State;
        take_away ->
            State - 1
    end