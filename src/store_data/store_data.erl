-module (store_data).

-export ([handle_request/2]).

-compile (export_all).

%% @spec handle_request(WebsocketSocket, PayloadContent) -> ok | {error, Reason}
handle_request(WebsocketSocket, PayloadContent) ->
    ok = gen_websocket:send(WebsocketSocket, PayloadContent),
    ok.