-module (store_data).

-export ([handle_data/2]).

-compile (export_all).

%% @spec handle_data(WebsocketSocket, PayloadContent) -> ok | {error, Reason}
handle_data(WebsocketSocket, PayloadContent) ->
    ok = gen_websocket:send(WebsocketSocket, PayloadContent),
    ok.