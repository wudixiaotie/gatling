-module (ws_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Mod, Args), {Mod, {Mod, start_link, Args}, temporary, brutal_kill, worker, [Mod]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(ListenSocket) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenSocket]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([ListenSocket]) ->
    case env:get(module) of
        {ok, Module} ->
            {ok, { {simple_one_for_one, 0, 1},
                   [?CHILD(websocket_server, [ListenSocket, Module])] } };
        _ ->
            io:format("You need to define callback module in src/gatling.app.src.~n"),
            ignore
    end.