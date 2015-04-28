-module(codd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pools} = application:get_env(codd, pools),
    lists:foreach(
        fun({PoolName, Driver, SizeArgs, WorkerArgs}) ->
            Driver:start_pool(PoolName, SizeArgs, WorkerArgs)
        end, Pools),
    codd_sup:start_link().

stop(_State) ->
    ok.
