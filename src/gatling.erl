-module (gatling).

-export ([listen/0, get_env/1, start_link/2]).

listen() ->
    Port = get_env(port),
    io:format("start websocket server: ws://localhost:~p~n", [Port]),
    Opts = [binary,
            {packet, 0},
            {reuseaddr, true},
            {active, true}],
    {ok, ListenSocket} = gen_tcp:listen(Port, Opts),
    ListenSocket.

get_env(Key) ->
    {ok, Value} = application:get_env(gatling, Key),
    Value.


start_link(ListenSocket, DispatcherName) ->
    gen_websocket:start_link(store_data, ListenSocket, DispatcherName).