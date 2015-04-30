-module (websocket_server1).

-export ([start/0, start/1, stop/0]).

-define (DEFAULT_PORT, 1987).
-compile(export_all).
% var list_ws = [];
% for(var i = 0; i < 100; i++) {
%   list_ws[i] = new WebSocket("ws://localhost:1987");
% }

start() ->
    start(?DEFAULT_PORT).


start(Port) ->
    register(?MODULE, spawn(fun() -> listen(Port) end)).


stop() ->
    case whereis(?MODULE) of
        undefined ->
            io:format("server is not running!~n");
        Pid ->
            Pid ! stop
    end.


listen(Port) ->
    process_flag(trap_exit, true),
    io:format("start websocket server: ws://localhost:~p~n", [Port]),
    Opts = [binary,
            {packet, 0},
            {reuseaddr, true},
            {active, true}],
    % ssl:start(),
    {ok, ListenSocket} = gen_tcp:listen(Port, Opts),
    spawn(fun() -> parallel_acceptor(ListenSocket) end),
    spawn(fun() -> parallel_acceptor(ListenSocket) end),
    receive
        _ ->
            io:format("listen~n")
    end.


parallel_acceptor(ListenSocket) ->
    io:format("linked parallel_accpeter:~p~n", [self()]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> parallel_acceptor(ListenSocket) end),
    % upgrade to ssl connection
    % inet:setopts(Socket, [{active, false}]),
    % {ok, SSLSocket} = ssl:ssl_accept(Socket,
    %                                 [{cacertfile, "cacerts.pem"},
    %                                  {certfile, "cert.pem"},
    %                                  {keyfile, "key.pem"}]),
    % inet:setopts(SSLSocket, [{active, true}]),
    % shake_hand(SSLSocket).
    shake_hand(Socket).


shake_hand(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            % io:format("receive_request received binary = ~p~n", [Bin]),
            HeaderList = binary:split(Bin, <<"\r\n">>, [global]),
            HeaderTupleList =[ list_to_tuple(binary:split(Header, <<": ">>)) || Header <- HeaderList ],
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
            frame_handler(Socket);
        Any ->
            io:format("receive_request received non_tcp: ~p.~n", [Any])
    end.


frame_handler(WebsocketSocket) ->
    receive
        {tcp, WebsocketSocket, FirstPacket} ->
            handle_data(FirstPacket, WebsocketSocket),
            frame_handler(WebsocketSocket);
        {tcp_closed, WebsocketSocket} ->
            gen_tcp:close(WebsocketSocket);
        Any ->
            io:format("websocket_handler received non_tcp:~p~n", [Any]),
            frame_handler(WebsocketSocket)
    end.


handle_data(FirstPacket, WebsocketSocket) ->
    {payload_original_data, PayloadOriginalData, next_packet_data, NextPacketData} = extract_payload_original_data(FirstPacket, WebsocketSocket),
    case unicode:characters_to_list(PayloadOriginalData) of
        {incomplete, _, _} ->
            gen_tcp:close(WebsocketSocket);
        PayloadContent ->
            Frame = build_frame(PayloadContent),
            gen_tcp:send(WebsocketSocket, Frame),
            case size(NextPacketData) of
                0 -> frame_handler(WebsocketSocket);
                _Other -> handle_data(NextPacketData, WebsocketSocket)
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