-module(mnesia_benchmark_srv).
-include_lib("core/include/metrics.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([update_histogram/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

update_histogram(Value) ->
    gen_server:cast(mnesia_benchmark_srv, {update_histogram, Value}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({update_histogram, Value}, State) ->
    case ?update_histogram([mnesia_benchmark, do_write_time], Value) of
        Value -> ok;
        T -> io:format("T ~p~n",[T])
    end,
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
