-module (env).

-export ([get/1]).


% @spec get(Key) -> Value | undefined
get (Key) ->
    case application:get_env (gatling, Key) of
        {ok, Value} -> Value;
        undefined -> undefined
    end.