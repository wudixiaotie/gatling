# gatling
## erlang websocket server
### ulimit system limit
ubuntu:  
####1.single file max handler:
```erl
erlang:system_info (check_io).
```
{max_fds,1024} shows how many handler you can set at one file.  
You can change that like this:
```shell
sudo vim /etc/security/limits.conf
```
add two rows:  
\*         soft    nofile  100000  
\*         hard    nofile  100000  
and restart your computer. It works! ye~ :).
####2.computer max port limit:
```erl
erlang:system_info (port_limit).
```
it shows max port limit of your computer, you can add +Q behind 'erl' like:
```shell
erl +Q 100000
```
to change it.
####3.computer port range:
if you don't need stress test then you can skip this step.  
```shell
su  
echo "1024 65535" > /proc/sys/net/ipv4/ip_local_port_range  
cat /proc/sys/net/ipv4/ip_local_port_range  
```

### how it works
-------
#### 1.Open a terminal and go to gatling direction(my was in ~/Documents/code/gatling/), and type:
```shell
rebar compile
erl
```
#### 2.start application
```erlang
gatling:start().
```
#### 3.Open gatling/client/gatling.html in browser

#### 4.you can set port in src/gatling.app.src by the name "port"

#### 5.each websocket server will auto stop when it have not been receive request from client for 1 hour. The time also can be custom set in src/gatling.app.src by the name "stop_time".

then you can see some log in browser's console. I only test in chrome, :).