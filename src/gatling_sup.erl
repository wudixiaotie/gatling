
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
            ?CHILD(websocket_dispatcher1, gatling, ListenSocket, worker),
            ?CHILD(websocket_dispatcher2, gatling, ListenSocket, worker),
            ?CHILD(websocket_dispatcher3, gatling, ListenSocket, worker)
          ]
         } }.


% init([ListenSocket]) ->
%     {ok, {
%           {simple_one_for_one, 0, 1},
%           [
%             ?CHILD(websocket_dispatcher1, gatling, ListenSocket, worker),
%             ?CHILD(websocket_dispatcher2, gatling, ListenSocket, worker),
%             ?CHILD(websocket_dispatcher3, gatling, ListenSocket, worker)
%           ]
%          } }.