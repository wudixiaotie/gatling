-module (gatling).

-export ([start_link/2, get_env/1]).


start_link(ListenSocket, DispatcherName) ->
    gen_websocket:start_link(store_data, ListenSocket, DispatcherName).


get_env(Key) ->
    {ok, Value} = application:get_env(gatling, Key),
    Value.