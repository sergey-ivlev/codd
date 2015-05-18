%%%-------------------------------------------------------------------
%%% @author isergey
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Apr 2015 11:57 AM
%%%-------------------------------------------------------------------
-module(codd_model).
-compile({parse_transform, do}).

-author("isergey").

%% API
-export([new/1, new/2]).
-export([meta/0, meta/1, meta/2]).
-export([changed_fields/1, is_from_db/1]).
-export([is_changed/2]).
-export([set/3, fields/2, value/2]).
-export([from_proplist/3, from_ext_proplist/3]).
-export([from_map/3, from_ext_map/3]).
-export([from_db/3]).
-export([to_ext_map/1, to_map/1, to_map/2]).
-export([to_ext_proplist/1, to_proplist/1]).
-export([find_alias/3, without_alias/1]).

%% common DB API
-export([get/2, get/3]).
-export([find/2, find/3, find/4]).
-export([save/1, save/2]).
-export([delete/1, delete/2]).
-export([db_keys/1]).

%% --------------------------------------
%% ------- callbacks --------------------
%% --------------------------------------
-callback def_kv() ->
    #{Key :: atom() => DefaultValue :: any()}.

-callback bin_to_key(BinKey :: binary()) ->
    Key :: atom().

-callback alias(Key :: atom()) ->
    AliasKey :: atom() |
    {AliasModule :: module(), AliasKey :: atom()}.

-callback is_db(Key :: atom()) ->
    DBFlag :: boolean().

-callback is_primary(Key :: atom()) ->
    PrimaryKeyFlag :: boolean().

-callback is_required(Key :: atom()) ->
    RequiredFlag :: boolean().

-callback is_r(Key :: atom()) ->
    UserReadFlag :: boolean().

-callback is_w(Key :: atom()) ->
    UserWriteFlag :: boolean().

-callback is_valid(Key :: atom(), Value :: any()) ->
    ValidFlag :: boolean().

-callback type(Key :: atom()) ->
    smallint | int2 | integer | int4 |bigint | int8 |
    real | float | float4 | float8 |
    string | text | varchar | binary |
    date | datetime |
    boolean | bool |
    list.

-callback db_table() ->
    Table :: binary().

-callback driver() ->
    StorageDriver :: atom().

%% --------------------------------------
%% ------- API --------------------------
%% --------------------------------------
new(Module)->
    new(Module, #{}).
new(Module, Opts)->
    do([error_m ||
        Meta <- meta(Opts),
        Model <- data(Module),
        {Module, Meta, Model}
    ]).

data(Module) ->
    DefData = Module:def_kv(),
    Fun = fun(Key,Value, {AccData, Errors}) ->
        case Value of
            undefined ->
                NewData = maps:put(Key, Value, AccData),
                {NewData, Errors};
            _ ->
                case find_alias(Module, Key, Value) of
                    {ok, {SourceValue, AliasValue}} ->
                        case Module:is_valid(Key, SourceValue) of
                            true ->
                                NewData = maps:put(Key, AliasValue, AccData),
                                {NewData, Errors};
                            false ->
                                {AccData, [codd_error:unvalid_error(Key,Value) | Errors]}
                        end;
                    {error, Reason} ->
                        {AccData, [Reason | Errors]}
                end


        end
    end,
    case maps:fold(Fun, {#{}, []}, DefData) of
        {Data, []} -> {ok, Data};
        {_, Errors} -> {error, Errors}
    end.

meta() ->
    meta(#{}).
meta(Opts) ->
    meta(Opts, def_meta()).
meta(Opts, Meta) ->
    Fun = fun(K, V, {Acc, Error}) ->
        case is_meta_option(K) of
            true ->
                case is_valid_meta_option(K,V) of
                    true ->
                        NewAcc = maps:update(K,V,Acc),
                        {NewAcc, Error};
                    false ->
                        {Acc, [codd_error:unvalid_option(K, V) | Error]}
                end;
            false ->
                {Acc, [codd_error:unknown_option(K) | Error]}
        end
    end,
    case maps:fold(Fun, {Meta, []}, Opts) of
        {Meta2, []} -> {ok, Meta2};
        {_, Errors} -> {error, Errors}
    end.


is_meta_option(from_db) -> true;
is_meta_option(changed_fields) -> true;
is_meta_option(_) -> false.

is_valid_meta_option(from_db, true) -> true;
is_valid_meta_option(from_db, false) -> true;
is_valid_meta_option(changed_fields, V) when is_map(V) ->  true;
is_valid_meta_option(_, _) -> false.

def_meta()->
    #{
        from_db => false,
        changed_fields => #{}
    }.
changed_fields({_, #{changed_fields := CF}, _}) ->
    CF.
is_from_db({_, #{from_db := FDB}, _}) ->
    FDB.

is_changed(Field, Model) ->
    CF = changed_fields(Model),
    case maps:find(Field, CF) of
        {ok, _} -> true;
        error -> false
    end.

%% --------------------------------------
%% ------- set api ----------------------
%% --------------------------------------
set(Key, Value,  {Module, Meta, Data}) ->
    case find_alias(Module, Key, Value) of
        {ok, {SourceValue, AliasValue}} ->
            case Module:is_valid(Key, SourceValue) of
                true ->
                    case maps:find(Key, Data) of
                        {ok, SourceValue} ->
                            {ok, {Module, Meta, Data}};
                        {ok, _} ->
                            Data2 = maps:update(Key, AliasValue, Data),
                            CF = maps:get(changed_fields, Meta),
                            NewCF = maps:put(Key, AliasValue, CF),
                            Meta2 = maps:put(changed_fields, NewCF, Meta),
                            {ok, {Module, Meta2, Data2}};
                        _ ->
                            {ok, {Module, Meta, Data}}
                    end;
                false ->
                    {error, codd_error:unvalid_error(Key, Value)}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

from_proplist(List, Opts, Model) ->
    Fun = fun({Key,Value}, {AccModel, Errors}) ->
               case set(Key, Value, AccModel) of
                   {ok, NewModel} ->
                       {NewModel, Errors};
                   {error, Error} ->
                        case maps:find(ignore_unknown, Opts) of
                            {ok, true} ->
                                {AccModel, Errors};
                            _ ->
                                {AccModel, [Error | Errors]}
                        end
                end
    end,
    case lists:foldl(Fun, {Model, []}, List) of
        {Data2, []} -> {ok, Data2};
        {_, Errors} -> {error, Errors}
    end.

from_ext_proplist(List, Opts, {Module, _, _} = Model) ->
    Fun = fun({BinKey,Value}, {AccModel, Errors}) ->
        case bin_to_key(Module, BinKey) of
            {ok, Key} ->
                case Module:is_w(Key) of
                    true ->
                        case set(Key, Value, AccModel) of
                            {error, Error} ->
                                {AccModel, [Error | Errors]};
                            {ok, NewModel} ->
                                {NewModel, Errors}
                        end;
                    false ->
                        case maps:find(ignore_unknown, Opts) of
                            {ok, true} ->
                                {AccModel, Errors};
                            _ ->
                                {AccModel, [codd_error:unknown_error(Key) | Errors]}
                        end
                end;
            {error, Error} ->
                {AccModel, [Error | Errors]}
        end
    end,
    case lists:foldl(Fun, {Model, []}, List) of
        {Data2, []} -> {ok, Data2};
        {_, Errors} -> {error, Errors}
    end.

from_map(Map, Opts, Model) ->
    Fun = fun(Key,Value, {AccModel, Errors}) ->
        case set(Key, Value, AccModel) of
            {ok, NewModel} ->
                {NewModel, Errors};
            {error, Error} ->
                case maps:find(ignore_unknown, Opts) of
                    {ok, true} ->
                        {AccModel, Errors};
                    _ ->
                        {AccModel, [Error | Errors]}
                end
        end
    end,
    case maps:fold(Fun, {Model, []}, Map) of
        {Model2, []} -> {ok, Model2};
        {_, Errors} -> {error, Errors}
    end.

from_ext_map(ExtMap, Opts, {Module, _, _} = Model) ->
    Fun = fun(BinKey,Value, {AccModel, Errors}) ->
        case bin_to_key(Module, BinKey) of
            {ok, Key} ->
                case Module:is_w(Key) of
                    true ->
                        case set(Key, Value, AccModel) of
                            {error, Error} ->
                                {AccModel, [Error | Errors]};
                            {ok, NewModel} ->
                                {NewModel, Errors}
                        end;
                    false ->
                        case maps:find(ignore_unknown, Opts) of
                            {ok, true} ->
                                {AccModel, Errors};
                            _ ->
                                {AccModel, [codd_error:unknown_error(Key) | Errors]}
                        end
                end;
            {error, Error} ->
                {AccModel, [Error | Errors]}
        end
    end,
    case maps:fold(Fun, {Model, []}, ExtMap) of
        {Model2, []} -> {ok, Model2};
        {_, Errors} -> {error, Errors}
    end.

from_db(DBPropList, _Opts, {Module, Meta, Data}) ->
    Fun = fun({BinKey,Value}, {AccModel, Errors}) ->
        case bin_to_key(Module, BinKey) of
            {ok, Key} ->
                case Module:is_db(Key) of
                    true ->
                        case find_alias(Module, Key, Value) of
                            {ok, {SourceValue, AliasValue}} ->
                                case Module:is_valid(Key, SourceValue) of
                                    true ->
                                        AccModel2 = maps:update(Key, AliasValue, AccModel),
                                        {AccModel2, Errors};
                                    false ->
                                        {AccModel, [codd_error:unvalid_error(Key, Value) | Errors]}
                                end;
                            {error, Reason} ->
                                {AccModel, [Reason | Errors]}
                        end;
                    false ->
                        {AccModel, [codd_error:unknown_error(Key) | Errors]}
                end;
            {error, Reason} ->
                {AccModel, [Reason | Errors]}
        end
    end,
    case lists:foldl(Fun, {Data, []}, DBPropList) of
        {Data2, []} -> {ok, {Module, Meta, Data2}};
        {_, Errors} -> {error, Errors}
    end.

value(Key, {_, _, Data}) ->
    maps:get(Key, Data).

fields(Key, {_, _, Data}) when is_list(Key) ->
    maps:with(Key, Data);
fields(Key, Model) ->
    fields([Key], Model).

to_ext_map(Model) ->
    Model2 = without_alias(Model),
    to_map(is_r, Model2).

to_map({_, _, Data})->
    Data.
to_map(Flag, {Module, _, Data}) ->
    Fun = fun(K,V, Acc) ->
        case Module:Flag(K) of
            true -> maps:put(K, V, Acc);
            false -> Acc
        end
    end,
    maps:fold(Fun,maps:new(),Data).

to_ext_proplist(Model) ->
    Map2 = to_ext_map(Model),
    maps:to_list(Map2).

to_proplist({?MODULE, _, Data}) ->
    maps:to_list(Data).

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
    case is_from_db(Model) of
        true ->
            Driver:update(Model);
        false ->
            Driver:insert(Model)
    end.

save(Connection, Model) ->
    Driver = Model:driver(),
    case is_from_db(Model) of
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

db_keys({Module, _Meta, Data}) ->
    [atom_to_binary(X, latin1) || X <- maps:keys(Data), Module:is_db(X)];
db_keys(Module) ->
    Data = Module:def_kv(),
    [atom_to_binary(X, latin1) || X <- maps:keys(Data), Module:is_db(X)].

without_alias({Module, #{changed_fields := CF}, Data}) ->
    do([error_m ||
        NewMeta <- without_alias(Module, CF),
        NewModel <- without_alias(Module, Data),
        {Module, NewMeta, NewModel}
    ]).
without_alias(Module, Map) ->
    Fun = fun(Key, Value, {AccMap, Errors}) ->
        case find_alias(Module, Key, Value) of
            {ok, {SourceValue, _}} ->
                NewMap = maps:put(Key, SourceValue, AccMap),
                {NewMap, Errors};
            {error, Reason} ->
                {AccMap, [Reason | Errors]}
        end
    end,
    case maps:fold(Fun, {#{}, []}, Map) of
        {Map2, []} -> {ok, Map2};
        {_, Errors} -> {error, Errors}
    end.

find_alias(Module, Key, Value) ->
    case Module:alias(Key) of
        no ->
            {ok, {Value, Value}};
        {AliasModule, AliasAtom} ->
            find_alias(AliasModule, AliasAtom, Value);
        List when is_list(List) ->
            FilterLIst = [{V,A} || {V,A} <- List, V =:= Value orelse A =:= Value],
            case FilterLIst of
                [] ->
                    {error, codd_error:alias_error(Key, Value)};
                [AliasPair | _] ->
                    {ok, AliasPair}
            end;
        AliasKey when is_atom(AliasKey) ->
            find_alias(Module, AliasKey, Value);
        _ ->
            {error, codd_error:alias_error(Key, Value)}
    end.

bin_to_key(Module, BinKey) ->
    try
        Key = Module:bin_to_key(BinKey),
        {ok, Key}
    catch
        error:_  ->
            codd_error:unknown_error(BinKey)
    end.
