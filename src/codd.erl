
%% @author @todo Add author to codd.
%% @doc @todo Add description to codd.

-module(codd).

%% ====================================================================
%% API functions
%% ====================================================================
%% common DB API
-export([get/2, get/3]).
-export([find/2, find/3, find/4]).
-export([save/1, save/2]).
-export([delete/1, delete/2]).
-export([count/2]).

get(Module, GetFields) when is_atom(Module) and is_map(GetFields) ->
    Driver = Module:driver(),
    Driver:get(Module, GetFields).
get(Connection, Module, GetFields) ->
    Driver = Module:driver(),
    Driver:get(Connection, Module, GetFields).

find(Module, FindCondition) ->
    find(Module, FindCondition, #{}).
find(Connection, Module, FindCondition) when is_pid(Connection) ->
    find(Connection, Module, FindCondition, #{});
find(Module, FindCondition, Opts) ->
    Driver = Module:driver(),
    Driver:find(Module, FindCondition, Opts).
find(Connection, Module, FindCondition, Opts) ->
    Driver = Module:driver(),
    Driver:find(Connection, Module, FindCondition, Opts).

save(Model)->
    Driver = Model:driver(),
    case codd_model:is_from_db(Model) of
        true ->
            Driver:update(Model);
        false ->
            Driver:insert(Model)
    end.

save(Connection, Model) ->
    Driver = Model:driver(),
    case codd_model:is_from_db(Model) of
        true ->
            Driver:update(Connection, Model);
        false ->
            Driver:insert(Connection, Model)
    end.

delete({Module, _,_} = Model) ->
    Driver = Module:driver(),
    Driver:delete(Model).
delete(Connection, {Module, _,_} = Model) ->
    Driver = Module:driver(),
    Driver:delete(Connection, Model).

count(Module, FindCondition) ->
    Driver = Module:driver(),
    Driver:count(Module, FindCondition).
%% ====================================================================
%% Internal functions
%% ====================================================================
