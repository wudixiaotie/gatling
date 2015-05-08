-module (websocket_server).

-behaviour (gen_server).

-export ([start/3, send_data/2, get_header/1]).

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record (state, {server_name,
                 websocket_socket,
                 header_tuple_list,
                 module,
                 timer_ref}).




%% APIs
start(Module, ListenSocket, DispatcherName) ->
    ServerName = {global, uuid:create()},
    spawn(fun() -> gen_server:start(ServerName,
                                    ?MODULE,
                                    [Module, ListenSocket, DispatcherName, ServerName],
                                    []) end).


% @spec send_data(ServerName, Data) -> ok | {stop, Reason}
send_data(ServerName, Data) ->
    gen_server:call(ServerName, {send_data, Data}).


get_header(ServerName) ->
    gen_server:call(ServerName, get_header).



%% gen_server callbacks
% init([Module, ListenSocket, DispatcherName]) -> {ok, State} | {stop, atom}
init([Module, ListenSocket, DispatcherName, ServerName]) ->
    io:format("start an new websocket server:~p~n", [ServerName]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> storage_server:start(ServerName) end),
    dispatcher:create_new_acceptor(DispatcherName),
    State = #state{ websocket_socket = Socket,
                    server_name = ServerName,
                    module = Module },
    shake_hand(State).


handle_call(get_header, _From, #state{header_tuple_list = HeaderTupleList} = State) ->
    {reply, HeaderTupleList, State};
handle_call({send_data, Data}, _Form, #state{websocket_socket = WebsocketSocket} = State) ->
    FunSendData = fun() ->
                    Frame = build_frame(Data),
                    gen_tcp:send(WebsocketSocket, Frame)
                  end,
    spawn(FunSendData),
    {reply, ok, State}.


handle_cast(_Msg, State) -> {noreply, State}.


handle_info({tcp, WebsocketSocket, FirstPacket}, #state{server_name = ServerName,
                                                        websocket_socket = WebsocketSocket,
                                                        module = Module,
                                                        timer_ref = TimerRef} = State) ->
    case erlang:cancel_timer(TimerRef) of
        false ->
            Reason = gatling:str("Server ~p can not cancel the timer!~n", [ServerName]),
            {stop, Reason, State};
        _ ->
            NewTimerRef = timer_callback(ServerName),
            spawn(fun() -> handle_data(ServerName, Module, FirstPacket, WebsocketSocket) end),
            NewState = State#state{timer_ref = NewTimerRef},
            {noreply, NewState}
    end;
handle_info({tcp_closed, WebsocketSocket}, #state{server_name = ServerName,
                                                  websocket_socket = WebsocketSocket} = State) ->
    before_stop(ServerName, WebsocketSocket),
    {stop, tcp_closed, State};
handle_info({stop, Reason}, #state{server_name = ServerName,
                                   websocket_socket = WebsocketSocket} = State) ->
    before_stop(ServerName, WebsocketSocket),
    {stop, Reason, State};
handle_info(Info, #state{server_name = ServerName} = State) ->
    io:format("websocket_server ~p received non_tcp:~p~n", [ServerName, Info]),
    {noreply, State}.


terminate(_Reason, #state{server_name = ServerName,
                          websocket_socket = WebsocketSocket}) ->
    before_stop(ServerName, WebsocketSocket).
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% internel functions
before_stop(ServerName, WebsocketSocket) ->
    io:format("websocket_server ~p stop!~n", [ServerName]),
    gen_tcp:close(WebsocketSocket),
    storage_server:stop(ServerName).



%% websocket protocol
% shake_hand(State) -> {ok, State} | {stop, atom}
shake_hand(#state{server_name = ServerName, websocket_socket = Socket} = State) ->
    receive
        {tcp, Socket, Bin} ->
            io:format("request header = ~p~n", [Bin]),
            HeaderList = binary:split(Bin, <<"\r\n">>, [global]),
            HeaderTupleList = [ list_to_tuple(binary:split(Header, <<": ">>)) || Header <- HeaderList ],
            {_, SecWebSocketKey} = lists:keyfind(<<"Sec-WebSocket-Key">>, 1, HeaderTupleList),
            Sha1 = crypto:hash(sha, [SecWebSocketKey, <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>]),
            Base64 = base64:encode(Sha1),
            HandshakeHeader = [
                <<"HTTP/1.1 101 Switching Protocols\r\n">>,
                <<"Upgrade: websocket\r\n">>,
                <<"Connection: Upgrade\r\n">>,
                <<"Sec-WebSocket-Accept: ">>, Base64, <<"\r\n">>,
                <<"\r\n">>
            ],
            ok = gen_tcp:send(Socket, HandshakeHeader),
            TimerRef = timer_callback(ServerName),
            NewState = State#state{header_tuple_list = HeaderTupleList,
                                   timer_ref = TimerRef},
            {ok, NewState};
        Any ->
            io:format("request received non_tcp: ~p.~n", [Any]),
            {stop, non_tcp_request}
    end.


handle_data(ServerName, Module, FirstPacket, WebsocketSocket) ->
    {payload_original_data, PayloadOriginalData, next_packet_data, NextPacketData} = extract_payload_original_data(FirstPacket, WebsocketSocket),
    case unicode:characters_to_list(PayloadOriginalData) of
        {incomplete, _, _} ->
            gen_tcp:close(WebsocketSocket);
        PayloadContent ->
            io:format("websocket_server ~p received data:~p~n", [ServerName, PayloadContent]),
            Module:handle_request(ServerName, PayloadContent),
            case size(NextPacketData) of
                0 -> ok;
                _Other -> handle_data(ServerName, Module, NextPacketData, WebsocketSocket)
            end
    end.


extract_payload_original_data(FirstPacket, WebsocketSocket) ->
    <<_Fin:1, _Rsv:3, _Opcode:4, _Mask:1, Len:7, Rest/binary>> = FirstPacket,
    case Len of
        126 ->
            <<PayloadLength:16, RestData/binary>> = Rest;
        127 ->
            <<PayloadLength:64, RestData/binary>> = Rest;
        _ ->
            PayloadLength = Len,
            RestData = Rest
    end,
    case PayloadLength > size(RestData) of
        true ->
            PayloadData = receive_rest_data(WebsocketSocket, PayloadLength, RestData);
        false ->
            PayloadData = RestData
    end,
    <<Masking:4/binary, MaskedData:PayloadLength/binary, NextPacketData/binary>> = PayloadData,
    PayloadOriginalData = unmask(MaskedData, Masking),
    {payload_original_data, PayloadOriginalData, next_packet_data, NextPacketData}.


receive_rest_data(WebsocketSocket, PayloadLength, ReceivedData) ->
    % masking length is 4
    case PayloadLength + 4 - size(ReceivedData) > 0  of
        true ->
            receive
                {tcp, WebsocketSocket, Packet} ->
                    NewReceivedData = list_to_binary([ReceivedData, Packet]),
                    receive_rest_data(WebsocketSocket, PayloadLength, NewReceivedData);
                {tcp_closed, WebsocketSocket} ->
                    gen_tcp:close(WebsocketSocket);
                Any ->
                    io:format("receive_rest_data received non_tcp:~p~n", [Any]),
                    receive_rest_data(WebsocketSocket, PayloadLength, ReceivedData)
            end;
        false ->
            ReceivedData
    end.


unmask(Payload, Masking) ->
    unmask(Payload, Masking, <<>>).


unmask(Payload, Masking = <<MA:8, MB:8, MC:8, MD:8>>, Acc) ->
    case size(Payload) of
        0 -> Acc;
        1 ->
            <<A:8>> = Payload,
            <<Acc/binary, (MA bxor A)>>;
        2 ->
            <<A:8, B:8>> = Payload,
            <<Acc/binary, (MA bxor A), (MB bxor B)>>;
        3 ->
            <<A:8, B:8, C:8>> = Payload,
            <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C)>>;
        _Other ->
            <<A:8, B:8, C:8, D:8, Rest/binary>> = Payload,
            Acc1 = <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C), (MD bxor D)>>,
            unmask(Rest, Masking, Acc1)
    end.


build_frame(Content) ->
    Bin = unicode:characters_to_binary(Content),
    DataLength = size(Bin),
    if
        DataLength =< 125 ->
            <<1:1, 0:3, 1:4, 0:1, DataLength:7, Bin/binary>>;
        (DataLength >= 125) and (DataLength =< 65535) ->
            <<1:1, 0:3, 1:4, 0:1, 126:7, DataLength:16, Bin/binary>>;
        DataLength > 65535 ->
            <<1:1, 0:3, 1:4, 0:1, 127:7, DataLength:64, Bin/binary>>
    end.



%% Timers
stop_time() ->
    case gatling:get_env(stop_time) of
        {ok, StopTime} -> StopTime;
        _ -> 3600000
    end.


timer_callback(ServerName) ->
    StopTime = stop_time(),
    Reason = gatling:str("Server ~p receive timeout from client!", [ServerName]),
    erlang:send_after(StopTime, self(), {stop, Reason}).