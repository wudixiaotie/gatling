
-module(gatling_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, Mod, Args, Type), {Name, {Mod, start_link, [Args, Name]}, permanent, 5000, Type, [Mod]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(ListenSocket) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenSocket]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

% init([]) ->
%     {ok, { {one_for_one, 5, 10}, []} }.


init([ListenSocket]) ->
    {ok, {
          {one_for_one, 5, 10},
          [
            ?CHILD(gen_websocket_worker1, gatling, ListenSocket, worker),
            ?CHILD(gen_websocket_worker2, gatling, ListenSocket, worker),
            ?CHILD(gen_websocket_worker3, gatling, ListenSocket, worker)
          ]
         } }.