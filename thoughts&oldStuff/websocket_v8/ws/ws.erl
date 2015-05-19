-module (ws).

-export ([start/1, send/2, get_header/1]).


start(WebsocketSocket) ->
    io:format("start an new websocket server:~n"),
    % supervisor:start_child(ws_sup, [Socket, data_storage]).
    websocket_server:start_link(WebsocketSocket, data_storage).


%% @spec send(ServerName, Data) -> ok | {error, Reason}
send(ServerName, Data) ->
    websocket_server:send_data(ServerName, Data).


get_header(ServerName) ->
    websocket_server:get_header(ServerName).