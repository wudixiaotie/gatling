-module (storage_server_sup).

-behaviour (supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(ChildID), {ChildID, {storage_server, start_link, [ChildID]}, permanent, 5000, worker, [storage_server]}).


start_link(ServerUUID) ->
    supervisor:start_link({ global, {storage_server_sup, ServerUUID} }, ?MODULE, [ServerUUID]).


init([ServerUUID]) ->
    {ok, {
          {one_for_one, 5, 10},
          [
            ?CHILD({ global, {storage_server, ServerUUID} })
          ]
         } }.