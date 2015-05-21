-module (ws_callback).

-export ([init/1, handle_request/2, stop/1]).


% callback during websocket server starting
init (Uuid) ->
    status:add (),
    ok.


% callback after websocket server received data
handle_request (Uuid, Data) ->
    ws:send (Uuid, Data).


% callback during websocket server stopping
stop (Uuid) ->
    status:remove (),
    ok.