# gatling
## erlang websocket server
### how it works
-------
* 1.Open a terminal and go to gatling direction(my was in ~/Documents/code/gatling/), and type:
```shell
rebar compile
erl
```
* 2.start application
```erlang
gatling:start().
```
* 3.Open gatling/client/gatling.html in browser

* 4.you can set port in src/gatling.app.src by the name "port"

* 5.each websocket server will auto stop when it have not been receive request from client for 1 hour. The time also can be custom set in src/gatling.app.src by the name "stop_time".

then you can see some log in browser's console. I only test in chrome, :).