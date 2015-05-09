-module (test).

-behaviour (gen_server).

-export ([start/0, stop/0]).

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

init([]) -> {ok, []}.
handle_call(_Msg, _From, State) -> {reply, _Msg, State}.
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
    ServerName = {websocket_server, "asdf"},
    gen_server:start_link({global, ServerName}, ?MODULE, [], []).
stop() ->
    gen_server:call(?MODULE, stop).