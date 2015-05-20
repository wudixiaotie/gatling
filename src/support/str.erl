-module (str).

-export ([format/1, format/2]).


format (Msg) ->
    lists:flatten (io_lib:format (Msg, [])).


format (Format, Args) ->
    lists:flatten (io_lib:format (Format, Args)).