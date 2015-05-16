
-module(gatling_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(ListenSocket) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenSocket]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([ListenSocket]) ->
    {ok, {
          {one_for_one, 5, 10},
          [
            {ws_sup, {ws_sup, start_link, [ListenSocket]}, permanent, 5000, supervisor, [ws_sup]}
          ]
         } }.