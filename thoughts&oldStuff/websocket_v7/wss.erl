-module (wss).

-export ([start/0, start/1, stop/0]).

-define (DEFAULT_PORT, 1987).
-compile(export_all).

-include ("test.hrl").

-define (TLS1_0, <<16#00:8, 16#03:8>>).

start() ->
    start(?DEFAULT_PORT).


start(Port) ->
    % register(?MODULE, spawn(fun() -> listen(Port) end)).
    register(?MODULE, spawn(fun() -> listen_ssl4(Port) end)).

timestamp() ->
    {M, S, _} = erlang:now(),
    M * 1000000 + S.
listen_ssl4(Port) ->
    io:format("start websocket secure server: wss://localhost:~p~n", [Port]),
    Opts = [binary,
            {packet, 0},
            {reuseaddr, true},
            {active, true}],
    {ok, ListenSocket} = gen_tcp:listen(Port, Opts),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    receive
        {tcp, Socket, Bin} ->
            <<ContentType:8, ?TLS1_0, Lenght:16, HandshakeProtocol/binary>> = Bin,
            % <<ContentType:8, Version:16, Lenght:16, HandshakeProtocol/binary>> = Bin,
            <<HandshakeType:8, HandshakeLength:24, HandshakeVersion:16,
              GmtUnixTime:4/binary, RandomBytes:28/binary,
              SessionIdLength:8, SessionId:SessionIdLength/binary,
              CipherSuitesLength:16, CipherSuites:CipherSuitesLength/binary,
              CompressionMethodsLegnth:8, CompressionMethods:CompressionMethodsLegnth/binary,
              ExtensionsLength:16, Extensions:ExtensionsLength/binary>> = HandshakeProtocol,
            io:format("ContentType:~p,~nVersion:~p,~nLenght:~p,~nHandshakeType:~p,~nHandshakeLength:~p,~nHandshakeVersion:~p,~nGmtUnixTime:~p,~nRandomBytes:~p,~nSessionIdLength:~p,~nSessionId:~p,~nCipherSuitesLength:~p,~nCipherSuites:~p,~nCompressionMethodsLegnth:~p,~nCompressionMethods:~p,~nExtensionsLength:~p,~nExtensions:~p,~n",
                      [ContentType, Version, Lenght, HandshakeType,
                       HandshakeLength, HandshakeVersion, GmtUnixTime, RandomBytes,
                       SessionIdLength, SessionId, CipherSuitesLength, CipherSuites,
                       CompressionMethodsLegnth, CompressionMethods, ExtensionsLength,
                       Extensions]);
        Any ->
            io:format("receive_request received non_tcp: ~p.~n", [Any])
    end.


listen(Port) ->
    process_flag(trap_exit, true),
    io:format("start websocket server: http://localhost:~p~n", [Port]),
    Opts = [binary,
            {packet, 0},
            {reuseaddr, true},
            {active, true}],
    {ok, ListenSocket} = gen_tcp:listen(Port, Opts),
    spawn(fun() -> parallel_connect(ListenSocket) end),
    receive
        {'EXIT', _Pid, Why} ->
            io:format("server http://localhost:~p has down, because ~p!~n", [Port, Why]),
            exit(stop)
    end.


listen_ssl(Port) ->
    io:format("start websocket secure server: wss://localhost:~p~n", [Port]),
    Opts = [binary,
            {certfile, "../../support/certificate_files/test_ssl_cert.pem"},
            {keyfile, "../../support/certificate_files/test_ssl_key.pem"},
            {packet, 0},
            {reuseaddr, true},
            {active, true}],
    {ok, Config} = test:handle_options(Opts, server),
    ConnectionCb = test:connection_cb(Opts),
    #config{transport_info = {Transport, _, _, _}, inet_user = Options, connection_cb = ConnectionCb} = Config,
    {ok, SecureListenSocket} = Transport:listen(Port, Options),
    {ok, SecureSocket} = Transport:accept(SecureListenSocket, infinity),
    receive
        {tcp, SecureSocket, Bin} ->
            io:format("receive_request received header = ~p~n", [Bin]),
            HeaderList = binary:split(Bin, <<"\r\n">>, [global]),
            HeaderTupleList = [ list_to_tuple(binary:split(Header, <<": ">>)) || Header <- HeaderList ],
            io:format("HeaderTupleList = ~p~n", [HeaderTupleList]),
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
            gen_tcp:send(SecureSocket, HandshakeHeader),
            frame_handler(SecureSocket);
        Any ->
            io:format("receive_request received non_tcp: ~p.~n", [Any])
    end.


listen_ssl1(Port) ->
    Port = 1987,
    ssl:start(),
    Opts = [binary,
            {certfile, "../../support/certificate_files/test_ssl_cert.pem"},
            {keyfile, "../../support/certificate_files/test_ssl_key.pem"},
            {packet, 0},
            {reuseaddr, true},
            {active, true}],
    {ok, SecureListenSocket} = ssl:listen(Port, Opts),
    {ok, SecureSocket} = ssl:transport_accept(SecureListenSocket),
    ok = ssl:ssl_accept(SecureSocket),
    io:format("asdfu~n").


listen_ssl2(Port) ->
    Port = 1987,
    ssl:start(),
    io:format("start websocket secure server: wss://localhost:~p~n", [Port]),
    Opts = [binary,
            {packet, 0},
            {reuseaddr, true},
            {active, true}],
    {ok, ListenSocket} = gen_tcp:listen(Port, Opts),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    inet:setopts(Socket, [{active, false}]),
    % Do the ssl handshake.
    CacertFile = "../../support/certificate_files/cacerts.pem",
    CertFile = "../../support/certificate_files/cert.pem",
    KeyFile = "../../support/certificate_files/key.pem",
    {ok, _SSLSocket} = ssl:ssl_accept(Socket,
                                     [{cacertfile, CacertFile},
                                      {certfile, CertFile},
                                      {keyfile, KeyFile}]).


parallel_connect(ListenSocket) ->
    io:format("linked parallel_connect:~p~n", [self()]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> parallel_connect(ListenSocket) end),
    shake_hand(Socket).


shake_hand(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            io:format("receive_request received header = ~p~n", [Bin]),
            HeaderList = binary:split(Bin, <<"\r\n">>, [global]),
            HeaderTupleList =[ list_to_tuple(binary:split(Header, <<": ">>)) || Header <- HeaderList ],
            SecWebSocketKey = proplists:get_value(<<"Sec-WebSocket-Key">>, HeaderTupleList),
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
            io:format("request data=~p~n", [PayloadContent]),
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


stop() ->
    case whereis(?MODULE) of
        undefined ->
            io:format("server not running!~n");
        Pid ->
            exit(Pid, stop)
    end.