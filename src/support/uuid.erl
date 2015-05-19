-module (uuid).

-export ([create/0]).

create () ->
    Base = {node (), make_ref (), now ()},
    <<I:160/integer>> = crypto:hash (sha, term_to_binary (Base)),
    str:format ("~40..0s", [erlang:integer_to_list (I, 16)]).