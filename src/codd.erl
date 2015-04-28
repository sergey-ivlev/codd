
%% @author @todo Add author to codd.
%% @doc @todo Add description to codd.

-module(codd).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, stop/0]).

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

%% ====================================================================
%% Internal functions
%% ====================================================================
