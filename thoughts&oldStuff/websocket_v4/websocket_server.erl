-module (websocket_server).
% 之所以不用gen_server是因为gen_server需要每个进程都要有单独的ServerName,
% 如果gen_server进程数量过多的化，会占用过多的atom,
% 而在一个 Erlang 节点内，atom 表是全局共享的,总数大概是1048576（1024×1024）个，
% 而且GC不会回收atom,
-export ([start/3, send_data/2]).



start(Module, ListenSocket, DispatcherName) ->
    spawn(fun() -> acceptor(Module, ListenSocket, DispatcherName) end).


% @spec send_data(WebsocketSocket, Data) -> ok | {error, Reason}
send_data(WebsocketSocket, Data) ->
    Frame = build_frame(Data),
    gen_tcp:send(WebsocketSocket, Frame).



%% internal functions
acceptor(Module, ListenSocket, DispatcherName) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    dispatcher:create_new_acceptor(DispatcherName),
    shake_hand(Module, Socket).


shake_hand(Module, Socket) ->
    receive
        {tcp, Socket, Bin} ->
            io:format("request header = ~p~n", [Bin]),
            io:format("request Socket = ~p~n", [Socket]),
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
            gen_tcp:send(Socket, HandshakeHeader),
            frame_handler(Module, Socket);
        Any ->
            io:format("request received non_tcp: ~p.~n", [Any])
    end.


frame_handler(Module, WebsocketSocket) ->
    receive
        {tcp, WebsocketSocket, FirstPacket} ->
            handle_data(Module, FirstPacket, WebsocketSocket),
            frame_handler(Module, WebsocketSocket);
        {tcp_closed, WebsocketSocket} ->
            gen_tcp:close(WebsocketSocket);
        Any ->
            io:format("websocket_handler received non_tcp:~p~n", [Any]),
            frame_handler(Module, WebsocketSocket)
    end.


handle_data(Module, FirstPacket, WebsocketSocket) ->
    {payload_original_data, PayloadOriginalData, next_packet_data, NextPacketData} = extract_payload_original_data(FirstPacket, WebsocketSocket),
    case unicode:characters_to_list(PayloadOriginalData) of
        {incomplete, _, _} ->
            gen_tcp:close(WebsocketSocket);
        PayloadContent ->
            Module:handle_request(WebsocketSocket, PayloadContent),
            case size(NextPacketData) of
                0 -> ok;
                _Other -> handle_data(Module, NextPacketData, WebsocketSocket)
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