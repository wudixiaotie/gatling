-module (store_data).

-export ([handle_data/2]).

-compile (export_all).

%% @spec handle_data(WebsocketSocket, PayloadContent) -> ok
handle_data(WebsocketSocket, PayloadContent) ->
    ok = websocket_server:send_data(WebsocketSocket, PayloadContent),
    ok.