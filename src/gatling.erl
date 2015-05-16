-module (gatling).

-export ([start/0]).

start() ->
    application:start(gatling),
    supervisor:start_child(ws_sup, []).