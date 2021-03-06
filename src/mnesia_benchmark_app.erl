%%%-------------------------------------------------------------------
%% @doc mnesia_benchmark public API
%% @end
%%%-------------------------------------------------------------------

-module(mnesia_benchmark_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    mnesia_benchmark_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
