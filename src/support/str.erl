-module (str).

-export ([format/2]).


format (Format, Args) ->
    lists:flatten (io_lib:format (Format, Args)).