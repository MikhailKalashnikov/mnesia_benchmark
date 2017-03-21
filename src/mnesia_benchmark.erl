-module(mnesia_benchmark).

%% API
-export([insert_test_data/1, init/1]).
-export([log_n/3, log_s_n/4, log_s_n/2]).
-record(test1, {id, value}).

init(Nodes) ->
    lists:foreach(fun net_kernel:connect_node/1, Nodes),
    lists:foreach(fun(X) -> rpc:call(X, mnesia, stop, []) end, Nodes),

    ok = mnesia:create_schema(Nodes),
    lists:foreach(fun(X) -> rpc:call(X, mnesia, start, []) end, Nodes),
    {atomic, ok} = mnesia:create_table(test1, [
        {disc_copies, Nodes},
        {type, ordered_set},
        {attributes, record_info(fields, test1)}
    ]),
    mnesia:wait_for_tables([test1], 50000).

insert_test_data(N) ->
    mnesia:wait_for_tables([test1], 50000),
    F = fun() ->
        lists:foreach(
            fun(X) ->
                mnesia:write(test1, #test1{id = X, value = 1}, write)
            end, lists:seq(1, N))
        end,
    mnesia:transaction(F).

log_n(Key, N, Sync) ->
    F = fun() -> lists:foreach(fun(_X) -> do_write(Key, Sync) end, lists:seq(1, N)) end,
    {T, _} = timer:tc(F),
    io:format("Writes (Sync ~p) ~p, Seconds = ~p ~n",  [Sync, N, round(T/1000000)]).


log_s_n(N, K) ->
    log_s_n(123, N, K, false).

log_s_n(Key, N, K, Sync) ->
    lists:foreach(
        fun(_X) ->
            spawn(?MODULE, log_n, [Key, K, Sync])
        end,
        lists:seq(1, N)).

do_write(Key, Sync) ->
    F = fun() ->
            [#test1{value = V} = R] = mnesia:wread({test1, Key}),
            mnesia:write(R#test1{value = V + 1})
        end,
    case Sync of
        true -> mnesia:sync_transaction(F);
        false -> mnesia:transaction(F)
    end.