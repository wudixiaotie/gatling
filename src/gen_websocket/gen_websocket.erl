-module (gen_websocket).

-export ([start_link/3]).

% @spec start_link(Module, ListenSocket, DispatcherName) -> {ok, Pid} | {error, Reason}
start_link(Module, ListenSocket, DispatcherName) ->
    dispatcher:start_link(Module, ListenSocket, DispatcherName).