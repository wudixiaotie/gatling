
-module (gatling_sup).

-behaviour (supervisor).

%% API
-export ([start_link/0]).

%% Supervisor callbacks
-export ([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link () ->
    supervisor:start_link ({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init ([]) ->
    {ok, {
          {one_for_one, 5, 10},
          [
            {ws_listener,
             {ws_listener, start_link, []}, permanent, 5000, worker, [ws_listener]},
            {status,
             {status, start_link, []}, permanent, 5000, worker, [status]},
            {ws_sup,
             {ws_sup, start_link, []}, permanent, 5000, supervisor, [ws_sup]}
          ]
         } }.