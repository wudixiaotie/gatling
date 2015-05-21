-module (client).

-compile (export_all).


start() ->
    {ok, Socket} = gen_tcp:connect({127,0,0,1}, 1987, [{packet,2}]).
    Header = request header = <<"GET / HTTP/1.1\r\nHost: localhost:1987\r\nConnection: Upgrade\r\nPragma: no-cache\r\nCache-Control: no-cache\r\nUpgrade: websocket\r\nOrigin: http://blog.yufeng.info\r\nSec-WebSocket-Version: 13\r\nUser-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.90 Safari/537.36\r\nAccept-Encoding: gzip, deflate, sdch\r\nAccept-Language: zh-CN,zh;q=0.8\r\nCookie: location_area_code=; location_name=\r\nSec-WebSocket-Key: ojSTLmSjcaQliHHSMimQTQ==\r\nSec-WebSocket-Extensions: permessage-deflate; client_max_window_bits\r\n\r\n">>,
    gen_tcp:send (Socket, Header)