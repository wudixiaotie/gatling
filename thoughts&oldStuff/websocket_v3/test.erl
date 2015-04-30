-module (test).

-behaviour (gen_server).

-export ([start_link/1, show/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
       terminate/2, code_change/3]).

%% APIs
start_link(ServerName) ->
    gen_server:start_link({local, ServerName}, ?MODULE, [], []).

show(Name) ->
    gen_server:call({show, Name}).


% gen_server callbacks
init([]) ->
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, 111, State}.


handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_info, State) -> {noreply, State}.
terminate(_Reason, _Status) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.