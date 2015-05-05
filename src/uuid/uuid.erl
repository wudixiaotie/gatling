-module (uuid).

-export ([create/0]).

create() ->
    <<I:160/integer>> = crypto:hash(sha, term_to_binary({make_ref(), now()})),
    lists:flatten(io_lib:format("~40..0s", [erlang:integer_to_list(I, 16)])).