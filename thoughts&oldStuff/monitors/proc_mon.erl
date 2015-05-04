-module (proc_mon).


lists:foreach(fun(T) -> io:format("~p:~p~n", [T,process_info(T)]) end, processes()).