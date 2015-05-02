-module(gatling_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Port = gatling:get_env(port),
    ListenSocket = gen_websocket:listen(Port),
    gatling_sup:start_link(ListenSocket).

stop(_State) ->
    ok.
