-module (client).

-compile (export_all).

start () ->
    start (1).


start (ID) ->
    io:format ("client No.~p start~n", [ID]),
    {ok, Socket} = gen_tcp:connect ({127,0,0,1}, 1987, [{packet,0}, {active,false}]),
    Header = [<<"GET / HTTP/1.1\r\n">>,
              <<"Host: localhost:1987\r\n">>,
              <<"Connection: Upgrade\r\n">>,
              <<"Pragma: no-cache\r\n">>,
              <<"Cache-Control: no-cache\r\n">>,
              <<"Upgrade: websocket\r\n">>,
              <<"Origin: http://blog.yufeng.info\r\n">>,
              <<"Sec-WebSocket-Version: 13\r\n">>,
              <<"User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.90 Safari/537.36\r\n">>,
              <<"Accept-Encoding: gzip, deflate, sdch\r\n">>,
              <<"Accept-Language: zh-CN,zh;q=0.8\r\n">>,
              <<"Cookie: location_area_code=; location_name=\r\n">>,
              <<"Sec-WebSocket-Key: ojSTLmSjcaQliHHSMimQTQ==\r\n">>,
              <<"Sec-WebSocket-Extensions: permessage-deflate; client_max_window_bits\r\n">>,
              <<"\r\n">>],
    gen_tcp:send (Socket, Header),
    gen_tcp:recv (Socket,0),
    Bin = <<1:1, 0:3, 1:4, 1:1, 3:7, 16#0f:8, 16#92:8, 16#69:8, 16#dc:8, 16#3e:8, 16#a0:8, 16#5a:8>>,
    gen_tcp:send (Socket, Bin),
    receive
      _ -> ok
    end.