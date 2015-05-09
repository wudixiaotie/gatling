-module (storage_server).

-behaviour (gen_server).

-export ([start_link/1, write/2]).

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record (state, {server_name, io_device}).



% Apis
start_link(ServerName) ->
    gen_server:start_link(ServerName, ?MODULE, [ServerName], []).


write(ServerUUID, Data) ->
    gen_server:cast({ global, {storage_server, ServerUUID} }, {write, Data}).



%% gen_server callbacks
% @spec init([{ global, {storage_server, ServerUUID} } = ServerName]) -> {ok, IoDevice} | {stop, Reason}
init([{ global, {storage_server, ServerUUID} } = ServerName]) ->
    FileName = string:concat(ServerUUID, ".data"),
    case file:open(FileName, [append]) of
        {error, Reason} ->
            {stop, Reason};
        {ok, IoDevice} ->
            State = #state{server_name = ServerName, io_device = IoDevice},
            {ok, State}
    end.


handle_call(_Msg, _From, State) -> {reply, _Msg, State}.


% @spec handle_cast(_Msg, State) -> {noreply, State} | {stop, normal, []}
handle_cast({write, Data},
            #state{server_name = ServerName, io_device = IoDevice} = State) ->
    io:format("StorageServer: ~p is writting ~p!~n", [ServerName, Data]),
    ok = file:pwrite(IoDevice, eof, string:concat(Data, "\n")),
    {noreply, State};
handle_cast(_Msg, State) -> {noreply, State}.


handle_info(_info, State) -> {noreply, State}.
% @spec terminate(_Reason, _State) -> ok | {error, Reason}
terminate(_Reason, #state{server_name = ServerName, io_device = IoDevice}) ->
    io:format("StorageServer: ~p is stoped!~n", [ServerName]),
    file:close(IoDevice).
code_change(_OldVsn, State, _Extraa) -> {ok, State}.