-module (gatling).

-export ([start_link/2, get_env/1, str/2]).


start_link(ListenSocket, DispatcherName) ->
    gen_websocket:start_link(data_storage, ListenSocket, DispatcherName).


% @spec get_env(Key) -> Value | undefined
get_env(Key) ->
    application:get_env(gatling, Key).


str(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).