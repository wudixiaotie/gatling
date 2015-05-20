-module (ws).

-behaviour (gen_server).

% APIs
-export ([start_link/1, send/2]).

% gen_server callbacks
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record (state, {uuid,
                 ws_socket,
                 ws_state, % shake_hand, ready, unfinished
                 unfinished_data = <<>>,
                 data_length = 0,
                 header_tuple_list,
                 module,
                 timer_ref}).








%%%------------------------------------------------------------------------
%%% APIs
%%%------------------------------------------------------------------------

% server function
start_link (WsSwocket) ->
    Uuid = uuid:create (),
    io:format ("start an new websocket server:~p~n", [Uuid]),
    State = #state{uuid = Uuid, ws_socket = WsSwocket, ws_state = shake_hand},
    gen_server:start_link ({global, {ws, Uuid}}, ?MODULE, [State], []).


% @spec send_data(Uuid, Data) -> ok | {stop, Reason}
send(Uuid, Data) ->
    gen_server:cast (server_name (Uuid), {send_data, Data}).





%%%------------------------------------------------------------------------
%%% gen_server callbacks
%%%------------------------------------------------------------------------

init ([State]) ->
    inet:setopts (State#state.ws_socket, [{active, once}, {packet, 0}, binary]),
    {ok, State}.


handle_call (Msg, _From, State) -> {reply, Msg, State}.


handle_cast ({send_data, Data}, #state{ws_socket = WsSwocket} = State) ->
    Frame = build_frame (Data),
    case gen_tcp:send (WsSwocket, Frame) of
        ok ->
            case reset_timer (State#state.timer_ref) of
                {error, Reason} ->
                    {stop, Reason, State};
                {ok, NewTimerRef} ->
                    {noreply, State#state{timer_ref = NewTimerRef}}
            end;
        {error, Reason} -> {stop, Reason, State}
    end;
handle_cast (_Msg, State) -> {noreply, State}.


% shake hand
handle_info ({tcp, WsSwocket, Bin}, #state{ws_socket = WsSwocket} = State)
    when State#state.ws_state =:= shake_hand ->
    io:format ("request header = ~p~n", [Bin]),
    HeaderList = binary:split (Bin, <<"\r\n">>, [global]),
    HeaderTupleList = [list_to_tuple (binary:split (Header, <<": ">>)) || Header <- HeaderList],
    SecWebSocketKey = proplists:get_value (<<"Sec-WebSocket-Key">>, HeaderTupleList),
    Sha = crypto:hash (sha, [SecWebSocketKey, <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>]),
    Base64 = base64:encode (Sha),
    HandshakeHeader = [
        <<"HTTP/1.1 101 Switching Protocols\r\n">>,
        <<"Upgrade: websocket\r\n">>,
        <<"Connection: Upgrade\r\n">>,
        <<"Sec-WebSocket-Accept: ">>, Base64, <<"\r\n">>,
        <<"\r\n">>
    ],
    ok = gen_tcp:send (WsSwocket, HandshakeHeader),
    TimerRef = timer_callback (),
    NewState = State#state{ws_state = ready,
                           header_tuple_list = HeaderTupleList,
                           timer_ref = TimerRef},
    setopts (NewState#state.ws_socket),
    {noreply, NewState};
handle_info ({tcp, WsSwocket, Bin}, #state{ws_socket = WsSwocket} = State)
    when State#state.ws_state =:= ready ->
    #state{uuid = Uuid, timer_ref = TimerRef} = State,
    case reset_timer (TimerRef) of
        {error, Reason} ->
            {stop, Reason, State};
        {ok, NewTimerRef} ->
            <<_Fin:1, _Rsv:3, _Opcode:4, _Mask:1, Len:7, Rest/binary>> = Bin,
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
                    NewState = State#state{ws_state = unfinished,
                                           unfinished_data = RestData,
                                           data_length = PayloadLength,
                                           timer_ref = NewTimerRef};
                false ->
                    NewState = State#state{timer_ref = NewTimerRef},
                    handle_data (RestData, Uuid)
            end,

            setopts (NewState#state.ws_socket),
            {noreply, NewState}
    end;
handle_info ({tcp, WsSwocket, Bin}, #state{ws_socket = WsSwocket} = State)
    when State#state.ws_state =:= unfinished ->
    #state{uuid = Uuid, data_length = DataLength, timer_ref = TimerRef} = State,
    case reset_timer (TimerRef) of
        {error, Reason} ->
            {stop, Reason, State};
        {ok, NewTimerRef} ->
            % masking length is 4
            ReceivedData = list_to_binary([State#state.unfinished_data, Bin]),
            case DataLength + 4 - size(ReceivedData) > 0  of
                true ->
                    NewState = State#state{unfinished_data = ReceivedData,
                                           timer_ref = NewTimerRef};
                false ->
                    NewState = State#state{ws_state = ready,
                                           unfinished_data = <<>>,
                                           data_length = 0,
                                           timer_ref = NewTimerRef},
                    handle_data (ReceivedData, Uuid)
            end,

            setopts (NewState#state.ws_socket),
            {noreply, NewState}
    end;
handle_info ({tcp_closed, WsSwocket}, #state{ws_socket = WsSwocket} = State) ->
    cleanup (State#state.uuid, WsSwocket),
    {stop, tcp_closed, State};
% after shake hand
handle_info (_Info, State) ->
    setopts (State#state.ws_socket),
    io:format("request received non_tcp: ~p.~n", [_Info]),
    {noreply, State}.


terminate (_Reason, State) ->
    cleanup (State#state.uuid, State#state.ws_socket),
    ok.
code_change (_OldVsn, State, _Extra) -> {ok, State}.





%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------

setopts (WsSwocket) ->
    inet:setopts (WsSwocket, [{active, once}, {packet, 0}, binary]).


cleanup (Uuid, WsSocket) ->
    io:format ("Server ~p stop!~n", [Uuid]),
    gen_tcp:close (WsSocket).
    % Module:stop(Uuid).


handle_data (PayloadData, Uuid) ->
    PayloadOriginalData = unmask (PayloadData),
    case unicode:characters_to_list (PayloadOriginalData) of
        {incomplete, _, _} ->
            io:format ("Server ~p can't decode data~n", [Uuid]);
        PayloadContent ->
            io:format ("Server ~p received data: ~p~n", [Uuid, PayloadContent]),
            % spawn (fun () -> send_data (Uuid, PayloadContent) end)
            send (Uuid, PayloadContent)
            % Module:handle_request(Uuid, PayloadContent)
    end.


unmask (PayloadData) ->
    <<Masking:4/binary, MaskedData/binary>> = PayloadData,
    unmask (MaskedData, Masking).


unmask (MaskedData, Masking) ->
    unmask (MaskedData, Masking, <<>>).


unmask (MaskedData, Masking = <<MA:8, MB:8, MC:8, MD:8>>, Acc) ->
    case size (MaskedData) of
        0 -> Acc;
        1 ->
            <<A:8>> = MaskedData,
            <<Acc/binary, (MA bxor A)>>;
        2 ->
            <<A:8, B:8>> = MaskedData,
            <<Acc/binary, (MA bxor A), (MB bxor B)>>;
        3 ->
            <<A:8, B:8, C:8>> = MaskedData,
            <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C)>>;
        _Other ->
            <<A:8, B:8, C:8, D:8, Rest/binary>> = MaskedData,
            Acc1 = <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C), (MD bxor D)>>,
            unmask (Rest, Masking, Acc1)
    end.


server_name (Uuid) ->
    {global, {ws, Uuid}}.


build_frame (Content) ->
    Bin = unicode:characters_to_binary (Content),
    DataLength = size (Bin),
    build_frame (DataLength, Bin).


build_frame (DataLength, Bin) when DataLength =< 125 ->
    <<1:1, 0:3, 1:4, 0:1, DataLength:7, Bin/binary>>;
build_frame (DataLength, Bin) when DataLength >= 125, DataLength =< 65535 ->
    <<1:1, 0:3, 1:4, 0:1, 126:7, DataLength:16, Bin/binary>>;
build_frame (DataLength, Bin) when DataLength > 65535 ->
    <<1:1, 0:3, 1:4, 0:1, 127:7, DataLength:64, Bin/binary>>.



%% Timers
reset_timer (TimerRef) ->
    case erlang:cancel_timer (TimerRef) of
        false ->
            Reason = str:format ("Server can not cancel the timer!~n"),
            {error, Reason};
        _ ->
            NewTimerRef = timer_callback (),
            {ok, NewTimerRef}
    end.


timer_callback () ->
    Reason = str:format ("Server receive timeout from client!"),
    erlang:send_after (stop_time (), self (), {stop, Reason}).


stop_time () ->
    case env:get (stop_time) of
        {ok, StopTime} -> StopTime;
        _ -> 3600000
    end.