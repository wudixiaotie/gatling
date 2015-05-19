-module (ws_listener).

-behaviour (gen_server).

-export ([start_link/0]).

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record (state, {listen_socket, ref}).


% server function
start_link () ->
    gen_server:start_link ({local, ?MODULE}, ?MODULE, [], []).



init ([]) ->
    Port = 1987,
    io:format ("start listen port: ~p~n", [Port]),
    Opts = [binary,
            {packet, 0},
            {reuseaddr, true},
            {keepalive, true},
            {backlog, 30},
            {active, false}],
    {ok, ListenSocket} = gen_tcp:listen (Port, Opts),
    {ok, Ref} = prim_inet:async_accept (ListenSocket, -1),
    State = #state{listen_socket = ListenSocket, ref = Ref},
    {ok, [State]}.


handle_call (_Msg, _From, State) -> {reply, _Msg, State}.
handle_cast (_Msg, State) -> {noreply, State}.


handle_info ({inet_async, State#state.listen_socket, State#state.ref, {ok, ClientSocket}}, State) ->
    true = inet_db:register_socket (WsSocket, inet_tcp),
    case set_sockopt (State#state.listen_socket, ClientSocket) of
        ok ->
            Pid = spawn (fun() -> server_loop(ClientSocket) end),
            gen_tcp:controlling_process (ClientSocket, Pid),
            {noreply, State};
        Error ->
            {stop, Error, State}
    end;
handle_info (_info, State) -> {noreply, State}.


terminate (_Reason, _State) ->
    gen_tcp:close(State#state.listen_socket),
    ok.
code_change (_OldVsn, State, _Extra) -> {ok, State}.



%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------

%% Taken from prim_inet.  We are merely copying some socket options from the
%% listening socket to the new Client socket.
set_sockopt (ListenSocket, ClientSocket) ->
    true = inet_db:register_socket (ClientSocket, inet_tcp),
    case prim_inet:getopts (ListSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
        {ok, Opts} ->
            case prim_inet:setopts (ClientSocket, Opts) of
                ok    -> ok;
                Error -> gen_tcp:close (ClientSocket), Error
            end;
        Error ->
            gen_tcp:close (ClientSocket), Error
    end.