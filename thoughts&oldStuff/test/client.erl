-module (client).

-compile (export_all).


start() ->
    Opts = [binary,
            {packet, 0},
            {reuseaddr, true},
            {active, true}],
    {ok, ListenSocket} = gen_tcp:listen(1987, Opts),
    {ok, Socket} = gen_tcp:accept(ListenSocket).