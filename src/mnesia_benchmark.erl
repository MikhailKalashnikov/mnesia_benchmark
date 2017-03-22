-module(mnesia_benchmark).

-include_lib("core/include/metrics.hrl").

%% API
-export([insert_test_data/1, init/1]).
-export([test_n/4, test_s_n/4, test_s_n/2]).
-export([test_s_nh/4, test_s_nh/2]).
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

test_n(Key, N, Sync, UpdateHistogram) ->
    F = fun() -> lists:foreach(fun(_X) -> do_write(Key, Sync, UpdateHistogram) end, 
        lists:seq(1, N)) end,
    {Micros, _} = timer:tc(F),
    ?update_histogram([mnesia_benchmark, proc_time], Micros div 1000),
    io:format("Writes (Sync ~p) ~p, Seconds = ~p ~n",  [Sync, N, Micros div 1000000]).


test_s_n(N, K) ->
    test_s_n(123, N, K, false).

test_s_n(Key, N, K, Sync) ->
    lists:foreach(
        fun(_X) ->
            spawn(?MODULE, test_n, [Key, K, Sync, false])
        end,
        lists:seq(1, N)).


test_s_nh(N, K) ->
    test_s_nh(123, N, K, false).

test_s_nh(Key, N, K, Sync) ->
    lists:foreach(
        fun(_X) ->
            spawn(?MODULE, test_n, [Key, K, Sync, true])
        end,
        lists:seq(1, N)).

do_write(Key, Sync, UpdateHistogram) ->
    F2 = fun() ->
        F = fun() ->
            [#test1{value = V} = R] = mnesia:wread({test1, Key}),
            mnesia:write(R#test1{value = V + 1})
            end,
        case Sync of
            true -> mnesia:sync_transaction(F);
            false -> mnesia:transaction(F)
        end
         end,
    {Micros, _} = timer:tc(F2),
    case UpdateHistogram of
        true -> ?update_histogram([mnesia_benchmark, do_write_time], Micros div 1000);
        _ -> ok
    end.
