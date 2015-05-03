-module (test).

-behaviour (gen_server).

-export ([start_link/0, show/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
       terminate/2, code_change/3]).

%% APIs
start_link() ->
    % gen_server:start_link({local, ServerName}, ?MODULE, [], []).
    gen_server:start_link({global, 123}, ?MODULE, [], []).

show(Name) ->
    gen_server:call({global, 123}, {show, Name}).


% gen_server callbacks
init([]) ->
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, 111, State}.


handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_info, State) -> {noreply, State}.
terminate(_Reason, _Status) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.