-module (exercises).

-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include ("tables.hrl").

reset_db() ->
    NodeList = node_list(),
    rpc_call(mnesia, stop, [], stopped),
    mnesia:delete_schema(NodeList),
    mnesia:create_schema(NodeList),
    rpc_call(mnesia, start, [], ok),
    {atomic, ok} = mnesia:create_table(users,
                                       [{attributes, record_info(fields, users)},
                                        {disc_copies, NodeList}]),
    {atomic, ok} = mnesia:create_table(tips,
                                       [{attributes, record_info(fields, tips)},
                                        {disc_copies, NodeList}]),
    {atomic, ok} = mnesia:create_table(abuse,
                                       [{attributes, record_info(fields, abuse)},
                                        {disc_copies, NodeList}]),
    rpc_call(mnesia, stop, [], stopped).

rpc_call(Module, Function, Args, Result) ->
    Fun = fun(Node) ->
        Result = rpc:call(Node, Module, Function, Args)
    end,
    lists:foreach(Fun, node_list()).

node_list() -> [node() | nodes()].

reset_data() ->
    add_user("zhangzhiyong", "zhangzhiyong@d2qb.com", "111111").

start() ->
    rpc_call(mnesia, start, [], ok),

query(Q) ->
    Trans = fun() -> qlc:e(Q) end,
    {atomic, Result} = mnesia:transaction(Trans),
    Result.

all(TableName) ->
    query(qlc:q([X || X <- mnesia:table(TableName)])).

add_user(Name, Email, Password) ->
    Row = #users{id = erlang:now(),
                 name = Name,
                 email = Email,
                 password = Password
                },
    Trans = fun() -> mnesia:write(Row) end,
    mnesia:transaction(Trans).

select_user_by({name, Name}) ->
    query(qlc:q([X || X <- mnesia:table(users), X#users.name =:= Name])).

delete_user(Id) ->
    Trans = fun() -> mnesia:delete({users, Id}) end,
    mnesia:transaction(Trans).

test() ->
    [User] = exercises:select_user_by({name, "zhangzhiyong"}),
    Id = User#users.id,
    {atomic, ok} = delete_user(Id).









