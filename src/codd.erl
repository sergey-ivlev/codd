
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
-export([delete/1, delete/2, delete/3]).
-export([count/2, count/3]).

% Connection, ?MODULE, #{id => Id, is_delete => false}
get(Module, GetFields) ->
    Adapter = Module:adapter(),
    Adapter:get(Module, GetFields).
get(Module, GetFields, Opts) ->
    Adapter = Module:adapter(),
    Adapter:get(Module, GetFields, Opts).

find(Module, FindCondition) ->
    Adapter = Module:adapter(),
    Adapter:find(Module, FindCondition).
find(Module, FindCondition, Opts) ->
    Adapter = Module:adapter(),
    Adapter:find(Module, FindCondition, Opts).

save(Model)->
    Adapter = Model:adapter(),
    case codd_model:is_from_db(Model) of
        true ->
            Adapter:update(Model);
        false ->
            Adapter:insert(Model)
    end.

save(Model, Opts) ->
    Adapter = Model:adapter(),
    case codd_model:is_from_db(Model) of
        true ->
            Adapter:update(Model, Opts);
        false ->
            Adapter:insert(Model, Opts)
    end.

delete(Model) ->
    Adapter = Model:adapter(),
    Adapter:delete(Model).
delete(Model, Opts) ->
    Adapter = Model:adapter(),
    Adapter:delete(Model, Opts).
delete(Module, Condition, Opts) ->
    Adapter = Module:adapter(),
    Adapter:delete(Module, Condition, Opts).

count(Module, FindCondition) ->
    Adapter = Module:adapter(),
    Adapter:count(Module, FindCondition).
count(Module, FindCondition, Opts) ->
    Adapter = Module:adapter(),
    Adapter:count(Module, FindCondition, Opts).
%% ====================================================================
%% Internal functions
%% ====================================================================
