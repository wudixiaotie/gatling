-module (gen_server_template).

-behaviour (gen_server).

-export ([start_link/0, stop/0]).

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).


init([]) -> {ok, []}.
handle_call(_Msg, _From, State) -> {reply, _Msg, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% server function
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() ->
    gen_server:cast(?MODULE, stop).