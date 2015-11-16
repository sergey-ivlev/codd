
%% @author @todo Add author to codd.
%% @doc @todo Add description to codd.

-module(codd).

%% ====================================================================
%% API functions
%% ====================================================================
%% common DB API
-export([get/2, get/3]).
-export([find/2, find/3]).
-export([save/1, save/2]).
-export([delete/1, delete/2]).
-export([count/2, count/3]).

% Connection, ?MODULE, #{id => Id, is_delete => false}
get(Module, GetFields) ->
    Driver = Module:driver(),
    Driver:get(Module, GetFields).
get(Module, GetFields, Opts) ->
    Driver = Module:driver(),
    Driver:get(Module, GetFields, Opts).

find(Module, FindCondition) ->
    Driver = Module:driver(),
    Driver:find(Module, FindCondition).
find(Module, FindCondition, Opts) ->
    Driver = Module:driver(),
    Driver:find(Module, FindCondition, Opts).

save(Model)->
    Driver = Model:driver(),
    case codd_model:is_from_db(Model) of
        true ->
            Driver:update(Model);
        false ->
            Driver:insert(Model)
    end.

save(Model, Opts) ->
    Driver = Model:driver(),
    case codd_model:is_from_db(Model) of
        true ->
            Driver:update(Model, Opts);
        false ->
            Driver:insert(Model, Opts)
    end.

delete({Module, _,_} = Model) ->
    Driver = Module:driver(),
    Driver:delete(Model).
delete({Module, _,_} = Model, Opts) ->
    Driver = Module:driver(),
    Driver:delete(Model, Opts).

count(Module, FindCondition) ->
    Driver = Module:driver(),
    Driver:count(Module, FindCondition).
count(Module, FindCondition, Opts) ->
    Driver = Module:driver(),
    Driver:count(Module, FindCondition, Opts).
%% ====================================================================
%% Internal functions
%% ====================================================================
