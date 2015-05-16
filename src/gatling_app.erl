-module(gatling_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case env:get(port) of
        {ok, Port} ->
            io:format("start websocket server: ws://localhost:~p~n", [Port]),
            Opts = [binary,
                    {packet, 0},
                    {reuseaddr, true},
                    {active, true}],
            {ok, ListenSocket} = gen_tcp:listen(Port, Opts),
            gatling_sup:start_link(ListenSocket);
        _ ->
            io:format("You need to define port in src/gatling.app.src.~n"),
            {stop, undefine_port}
    end.

stop(_State) ->
    ok.
