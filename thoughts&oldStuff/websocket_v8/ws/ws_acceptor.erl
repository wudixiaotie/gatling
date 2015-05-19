-module (ws_acceptor).

-export ([start_link/1]).


start_link(ListenSocket) ->
    io:format("start websocket acceptor!~n"),
    {ok, spawn_link(fun() -> loop(ListenSocket) end)}.


loop(ListenSocket) ->
    {ok, WebsocketSocket} = gen_tcp:accept(ListenSocket),
    ws:start(WebsocketSocket),
    loop(ListenSocket).