%%%-------------------------------------------------------------------
%% @doc mnesia_benchmark top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mnesia_benchmark_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 0, 1}, [
        #{id => mnesia_benchmark_srv, start => {mnesia_benchmark_srv, start_link, []}}]} }.

%%====================================================================
%% Internal functions
%%====================================================================
