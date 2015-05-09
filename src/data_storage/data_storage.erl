-module (data_storage).

-export ([start_link/1, stop/1, handle_request/2]).


start_link(ServerUUID) ->
    storage_server_sup:start_link(ServerUUID).


stop(ServerUUID) ->
    exit(global:whereis_name({storage_server_sup, ServerUUID}), kill).


%% @spec handle_request(ServerName, PayloadContent) -> ok | {error, Reason}
handle_request({global, {websocket_server, ServerUUID} }, PayloadContent) ->
    % Header = gen_websocket:get_header(ServerName),
    % io:format("~p~n", [Header]),
    % ok = gen_websocket:send(ServerName, PayloadContent),
    storage_server:write(ServerUUID, PayloadContent).