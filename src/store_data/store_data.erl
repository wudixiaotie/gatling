-module (store_data).

-export ([handle_request/2]).

-compile (export_all).

%% @spec handle_request(ServerName, PayloadContent) -> ok | {error, Reason}
handle_request(ServerName, PayloadContent) ->
    % Header = gen_websocket:get_header(ServerName),
    % io:format("~p~n", [Header]),
    ok = gen_websocket:send(ServerName, PayloadContent),
    ok.