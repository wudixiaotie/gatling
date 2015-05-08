-module (storage_server).

-behaviour (gen_server).

-export ([start/1, stop/1, handle_request/2]).

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).



% apis
start({global, ServerUUID}) ->
    gen_server:start_link({ global, {ServerUUID, storage_server} },
                          ?MODULE,
                          [ServerUUID],
                          []).


stop({global, ServerUUID}) ->
    gen_server:cast({ global, {ServerUUID, storage_server} }, stop).


%% @spec handle_request(ServerName, PayloadContent) -> ok | {error, Reason}
handle_request({global, ServerUUID}, PayloadContent) ->
    % Header = gen_websocket:get_header(ServerName),
    % io:format("~p~n", [Header]),
    % ok = gen_websocket:send(ServerName, PayloadContent),
    gen_server:cast({ global, {ServerUUID, storage_server} },
                    {write, PayloadContent}).



%% gen_server callbacks
% @spec init([ServerUUID]) -> {ok, IoDevice} | {stop, Reason}
init([ServerUUID]) ->
    FileName = string:concat(ServerUUID, ".data"),
    case file:open(FileName, [append]) of
        {error, Reason} ->
            {stop, Reason};
        {ok, IoDevice} ->
            {ok, [IoDevice]}
    end.


handle_call(_Msg, _From, State) -> {reply, _Msg, State}.


% @spec handle_cast(_Msg, State) -> {noreply, State} | {stop, normal, []}
handle_cast({write, Data}, [IoDevice]) ->
    ok = file:pwrite(IoDevice, eof, string:concat(Data, "\n")),
    {noreply, [IoDevice]};
handle_cast(stop, [IoDevice]) ->
    ok = file:close(IoDevice),
    {stop, normal, []};
handle_cast(_Msg, State) -> {noreply, State}.


handle_info(_info, State) -> {noreply, State}.
% @spec terminate(_Reason, _State) -> ok | {error, Reason}
terminate(_Reason, [IoDevice]) -> file:close(IoDevice).
code_change(_OldVsn, State, _Extraa) -> {ok, State}.