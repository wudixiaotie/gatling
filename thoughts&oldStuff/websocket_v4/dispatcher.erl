-module (dispatcher).

-behaviour (gen_server).

-export ([start_link/3, create_new_acceptor/1]).

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record (state, {module, listen_socket, dispatcher_name}).




%% APIs
start_link(Module, ListenSocket, DispatcherName) ->
    State = #state{ module = Module,
                    listen_socket = ListenSocket,
                    dispatcher_name = DispatcherName },
    gen_server:start_link({local, DispatcherName}, ?MODULE, [State], []).


create_new_acceptor(DispatcherName) ->
    gen_server:call(DispatcherName, create_new_acceptor).




% gen_server callbacks

init([#state{ module = Module,
              listen_socket = ListenSocket,
              dispatcher_name = DispatcherName } = State]) ->
    websocket_server:start(Module, ListenSocket, DispatcherName),
    io:format("start dispatcher name:~p~n", [DispatcherName]),
    {ok, State}.


handle_call(create_new_acceptor, _From,
            #state{ module = Module,
                    listen_socket = ListenSocket,
                    dispatcher_name = DispatcherName } = State) ->
    websocket_server:start(Module, ListenSocket, DispatcherName),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ignore, State}.


handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_info, State) -> {noreply, State}.
terminate(_Reason, _Status) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.