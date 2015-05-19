-module (server).

-compile (export_all).


start() ->
    Port = 2222,
    Opts = [binary, {packet, 0}, {reuseaddr, true},
            {keepalive, true}, {backlog, 30}, {active, false}],
    {ok, ListenSocket} = gen_tcp:listen (Port, Opts),
    {ok, Ref} = prim_inet:async_accept (ListenSocket, -1),
    listen_loop(ListenSocket, Ref).


listen_loop(ListenSocket, Ref) ->
    receive
        {inet_async, ListenSocket, Ref, {ok, ClientSocket}} ->
            true = inet_db:register_socket (ClientSocket, inet_tcp),
            {ok, Opts} = prim_inet:getopts (ListenSocket, [active, nodelay, keepalive, delay_send, priority, tos]),
            ok = prim_inet:setopts (ClientSocket, Opts),
    io:format("listen_loop:~p~n", [ClientSocket]),
            Pid = spawn(fun() -> server_loop(ClientSocket) end),
            gen_tcp:controlling_process (ClientSocket, Pid),
            listen_loop(ListenSocket, Ref);
        Any ->
            io:format("~p~n", [Any])
    end.


server_loop(Socket) ->
    io:format("server_loop:~p~n", [Socket]),
    inet:setopts (Socket, [{active, once}, {packet, 0}, binary]),
    {ok, {IP, Port}} = inet:peername (Socket),
    receive
        {tcp, Socket, Bin} ->
            io:format("~p~n", [Bin]);
        Any ->
            io:format("~p~n", [Any])
    end,
    server_loop(Socket).