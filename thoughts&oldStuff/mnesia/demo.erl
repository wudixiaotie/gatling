-module (demo).

% -export ([table_definition/0, start/0, stop/0, reset_tables/0, demo/1]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-record(shop, {item, quantity, cost}).
-record(cost, {name, price}).

table_definition() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    {atomic,ok} = mnesia:create_table(shop, [{attributes, record_info(fields, shop)}]),
    {atomic,ok} = mnesia:create_table(cost, [{attributes, record_info(fields, cost)}]),
    mnesia:stop().

start() ->
    mnesia:start().

stop() ->
    mnesia:stop().

reset_tables() ->
    TestData = 
    [%% The shop table
     {shop, apple, 20, 2.3},
     {shop, orange, 100, 3.8},
     {shop, pear, 200, 3.6},
     {shop, banana, 420, 4.5},
     {shop, potato, 2456, 1.2},
     %% The cost table
     {cost, apple,   1.5},
     {cost, orange,  2.4},
     {cost, pear,    2.2},
     {cost, banana,  1.5},
     {cost, potato,  0.6}
    ],
    mnesia:clear_table(shop),
    mnesia:clear_table(cost),
    F = fun() ->
            lists:foreach(fun mnesia:write/1, TestData)
            % [mnesia:write(Row) || Row <- TestData]
        end,
    mnesia:transaction(F).


%% SQL equivalent
%%  SELECT * FROM shop;
demo(select_shop) ->
    do(qlc:q([X || X <- mnesia:table(shop)]));

%% SQL equivalent
%%  SELECT item, quantity FROM shop;
demo(select_some) ->
    do(qlc:q([{X#shop.item, X#shop.quantity} || X <- mnesia:table(shop)]));

%% SQL equivalent
%%   SELECT shop.item FROM shop
%%   WHERE  shop.quantity < 250;
demo(reorder) ->
    do(qlc:q([X#shop.item || X <- mnesia:table(shop),
                             X#shop.quantity < 250]));

%% SQL equivalent
%%  SELECT shop.item
%%  FROM shop, cost
%%  WHERE shop.item = cost.name
%%      AND cost.price < 2
%%      AND shop.quantity < 250
demo(join) ->
    do(qlc:q([X#shop.item || X <- mnesia:table(shop),
                             X#shop.quantity < 250,
                             Y <- mnesia:table(cost),
                             X#shop.item =:= Y#cost.name,
                             Y#cost.price < 2])).

add_shop_item(Name, Quantity, Cost) ->
    Row = #shop{item=Name, quantity=Quantity, cost=Cost},
    F = fun() -> mnesia:write(Row) end,
    mnesia:transaction(F).

remove_shop_item(Item) ->
    Oid = {shop, Item},
    F = fun() -> mnesia:delete(Oid) end,
    mnesia:transaction(F).

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

































