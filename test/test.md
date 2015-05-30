测试计划备忘：
1.启动6000个client链接server，每个client每秒发送一条消息，看看server负载情况。
client shell CPU：12% MEM：168
server shell CPU：12% MEM：297
server每条消息的处理时间在5ms以内
2.启动12000个client链接server，每个client每秒发送一条消息，看看server负载情况。
client shell CPU：12% MEM：216
server shell CPU：28% MEM：494
server每条消息的处理时间在7-13ms之间
2.启动16000个client链接server，每个client每秒发送一条消息，看看server负载情况。
client shell CPU：12% MEM：216
server shell CPU：28% MEM：494
server每条消息的处理时间在7-13ms之间
2.去掉server的timer看看是否能够提高性能。
3.