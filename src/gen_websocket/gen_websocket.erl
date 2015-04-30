-module (gen_websocket).

-behaviour (gen_server).

-export ([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
       terminate/2, code_change/3]).

%% APIs
start_link(ListenSocket) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ListenSocket], []).




% gen_server callbacks
init([ListenSocket]) ->
    F = fun() ->
            DispatcherPid = self(),
            spawn(fun() -> acceptor(ListenSocket, DispatcherPid) end),
            dispatcher(ListenSocket)
        end,
    Pid = spawn_link(F),
    io:format("linked parallel_accpeter:~p~n", [Pid]),
    State = {listen_socket, ListenSocket},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, Reply, State}.


handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_info, State) -> {noreply, State}.
terminate(_Reason, _Status) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.