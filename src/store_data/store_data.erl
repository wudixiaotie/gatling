-module (store_data).

-export ([handle_request/2]).

-compile (export_all).

%% @spec handle_request(WebsocketSocket, PayloadContent) -> ok | {error, Reason}
handle_request(WebsocketSocket, PayloadContent) ->
    Header = gen_websocket:get_header(self()),
    io:format("~p~n", [Header]),
    ok = gen_websocket:send(WebsocketSocket, PayloadContent),
    ok.