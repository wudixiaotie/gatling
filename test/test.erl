-module (test).


-export ([start/1]).

start (Max) ->
    loop (0, Max).


% loop (Count, Max) ->
%     case Count > Max of
%         true -> stop;
%         false ->
%             spawn (fun () -> client:start (Count) end),
%             erlang:send_after (10, self(), go)
%     end,
%     receive
%         _ -> loop (Count + 1, Max)
%     after 5 * 1000 -> stop
%     end.


loop (Count, Max) ->
    case Count > Max of
        true -> stop;
        false ->
            spawn (fun () -> client:start (Count) end),
            loop (Count + 1, Max)
    end.