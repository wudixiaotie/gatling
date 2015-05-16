-module (env).

-export ([get/1]).


% @spec get(Key) -> Value | undefined
get(Key) ->
    application:get_env(gatling, Key).