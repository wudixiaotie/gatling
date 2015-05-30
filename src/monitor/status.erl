-module (status).

-behaviour (gen_server).

-export ([start_link/0, now/0, add/0, remove/0]).

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record (state, {ws_server_count = 0}).


start_link () ->
    gen_server:start_link ({local, ?MODULE}, ?MODULE, [], []).


now () ->
    gen_server:call (?MODULE, now).


add () ->
    gen_server:cast (?MODULE, add).


remove () ->
    gen_server:cast (?MODULE, remove).





%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init ([]) -> io:format("status server start~n"),{ok, #state{}}.


handle_call (now, _From, State) -> {reply, State#state.ws_server_count, State};
handle_call (Mag, _From, State) -> {reply, Mag, State}.


handle_cast (add, State) ->
    NewCount = State#state.ws_server_count + 1,
    NewState = State#state{ws_server_count = NewCount},
    {noreply, NewState};
handle_cast (remove, State) ->
    NewCount = State#state.ws_server_count - 1,
    NewState = State#state{ws_server_count = NewCount},
    {noreply, NewState};
handle_cast (_Msg, State) -> {noreply, State}.


handle_info (_Info, State) -> {noreply, State}.
terminate (_Reason, _State) -> ok.
code_change (_OldVsn, State, _Extra) -> {ok, State}.