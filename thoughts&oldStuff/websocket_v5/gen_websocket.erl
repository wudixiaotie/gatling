-module (gen_websocket).

-export ([start_link/3, listen/1, send/2]).

% @spec start_link(Module, ListenSocket, DispatcherName) -> {ok, Pid} | {error, Reason}
start_link(Module, ListenSocket, DispatcherName) ->
    dispatcher:start_link(Module, ListenSocket, DispatcherName).


listen(Port) ->
    io:format("start websocket server: ws://localhost:~p~n", [Port]),
    Opts = [binary,
            {packet, 0},
            {reuseaddr, true},
            {active, true}],
    {ok, ListenSocket} = gen_tcp:listen(Port, Opts),
    ListenSocket.


%% @spec send(WebsocketSocket, Data) -> ok | {error, Reason}
send(WebsocketSocket, Data) ->
    websocket_server:send_data(WebsocketSocket, Data).
