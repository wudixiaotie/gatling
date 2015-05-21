-module (test1).

-behaviour (gen_server).

-export ([start/0, stop/0]).

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

init([]) ->
    io:format("init:~p~n", [self()]),
    receive
        _ -> ok
    end,
    {ok, []}.
handle_call(_Msg, _From, State) ->
    io:format("handle_call:~p~n", [self()]),
    {reply, _Msg, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(stop, State) ->
    io:format("========got message!~p~n", [stop]),
    {stop, normal, State};
handle_info(error, State) ->
    io:format("========got error!~p~n", [error]),
    erlang:error(fuck),
    {noreply, State};
handle_info(_Info, State) ->
    io:format("========got message!~p~n", [_Info]),
    {noreply, State}.
terminate(_Reason, _Status) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% server function
start() ->
    io:format("start_link:~p~n", [self()]),
    ServerName = {websocket_server, "asdf"},
    gen_server:start_link({global, ServerName}, ?MODULE, [], []).
stop() ->
    gen_server:call(?MODULE, stop).