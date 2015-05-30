-module (ws_listener).

-behaviour (supervisor).

%% API
-export ([start_link/0]).

%% Supervisor callbacks
-export ([init/1]).

%% Helper macro for declaring children of supervisor
-define (CHILD(Mod, Args), {Mod, {Mod, start_link, Args}, temporary, brutal_kill, worker, [Mod]}).

%% Default port
-define (PORT, 1987).


%% ===================================================================
%% API functions
%% ===================================================================

start_link () ->
    supervisor:start_link ({local, ?MODULE}, ?MODULE, []).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init ([]) ->
    {ok, ListenSocket} = listen (),
    {ok, { {simple_one_for_one, 0, 1}, [?CHILD(ws_acceptor, [ListenSocket])] } }.


%% ===================================================================
%% internal functions
%% ===================================================================

-spec listen () -> {ok, _ListenSocket} | {error, _Msg}.
listen () ->
    case env:get(port) of
        undefined ->
            Port = ?PORT;
        Port ->
            ok
    end,
    io:format ("start listen port: ~p~n", [Port]),
    Opts = [binary,
            {packet, 0},
            {reuseaddr, true},
            {keepalive, true},
            {backlog, 60000},
            {active, false}],
    gen_tcp:listen (Port, Opts).