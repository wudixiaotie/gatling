-module(gatling_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case gatling:get_env(port) of
        {ok, Port} ->
            ListenSocket = gen_websocket:listen(Port),
            gatling_sup:start_link(ListenSocket);
        _ ->
            io:format("You need to define port in src/gatling.app.src.~n")
    end.

stop(_State) ->
    ok.
