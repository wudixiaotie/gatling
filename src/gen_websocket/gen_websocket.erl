-module (gen_websocket).

-export ([start_link/2]).


start_link(ListenSocket, DispatcherName) ->
    dispatcher:start_link(ListenSocket, DispatcherName).