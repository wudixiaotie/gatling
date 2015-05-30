-module (ws_acceptor).

-behaviour (gen_server).

-export ([start_link/1]).

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record (state, {listen_socket, acceptor_ref}).


% server function
start_link (ListenSocket) ->
    gen_server:start_link (?MODULE, [ListenSocket], []).



init ([ListenSocket]) ->
    {ok, AcceptorRef} = prim_inet:async_accept (ListenSocket, -1),
    State = #state{listen_socket = ListenSocket, acceptor_ref = AcceptorRef},
    {ok, State}.


handle_call (_Msg, _From, State) -> {reply, _Msg, State}.
handle_cast (_Msg, State) -> {noreply, State}.


handle_info ({inet_async, ListenSocket, AcceptorRef, {ok, ClientSocket}},
             #state{listen_socket = ListenSocket, acceptor_ref = AcceptorRef} = State) ->
    true = inet_db:register_socket (ClientSocket, inet_tcp),
    case set_sockopt (State#state.listen_socket, ClientSocket) of
        ok ->
            {ok, Pid} = supervisor:start_child(ws_sup, [ClientSocket]),
            gen_tcp:controlling_process (ClientSocket, Pid),
            case prim_inet:async_accept (ListenSocket, -1) of
                {ok,    NewAcceptorRef} -> ok;
                {error, NewAcceptorRef} ->
                    io:format("status:now() => ~p~n", [status:now()]),
                    exit ({async_accept, inet:format_error (NewAcceptorRef)})
            end,
            NewState = State#state{acceptor_ref = NewAcceptorRef},
            {noreply, NewState};
        Error ->
            {stop, Error, State}
    end;
handle_info (_Info, State) ->
    io:format("_Info:~p~n", [_Info]),
    {noreply, State}.


terminate (_Reason, State) ->
    gen_tcp:close(State#state.listen_socket),
    ok.
code_change (_OldVsn, State, _Extra) -> {ok, State}.



%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------

%% Taken from prim_inet.  We are merely copying some socket options from the
%% listening socket to the new TCP socket.
set_sockopt (ListenSocket, ClientSocket) ->
    true = inet_db:register_socket (ClientSocket, inet_tcp),
    case prim_inet:getopts (ListenSocket, [active, nodelay, keepalive, delay_send, priority, tos]) of
        {ok, Opts} ->
            case prim_inet:setopts (ClientSocket, Opts) of
                ok    -> ok;
                Error -> gen_tcp:close (ClientSocket), Error
            end;
        Error ->
            gen_tcp:close (ClientSocket), Error
    end.