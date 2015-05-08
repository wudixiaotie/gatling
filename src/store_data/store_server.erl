-module (store_server).

-behaviour (gen_server).

-export ([start/0, stop/0, handle_request/2]).

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).



% apis
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


stop() ->
    gen_server:call(?MODULE, stop).


%% @spec handle_request(ServerName, PayloadContent) -> ok | {error, Reason}
handle_request(ServerName, PayloadContent) ->
    % Header = gen_websocket:get_header(ServerName),
    % io:format("~p~n", [Header]),
    ok = gen_websocket:send(ServerName, PayloadContent),
    ok.



%% gen_server callbacks
init([]) -> {ok, []}.
handle_call(_Msg, _From, State) -> {reply, _Msg, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_info, State) -> {noreply, State}.
terminate(_Reason, _Status) -> ok.
code_change(_OldVsn, State, _Extraa) -> {ok, State}.