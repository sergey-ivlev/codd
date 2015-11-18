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

-export([from_db/2]).

-export([to_ext_map/1, to_map/1, to_map/2]).
-export([to_ext_proplist/1, to_proplist/1]).

-export([db_keys/1, db_keys/2]).


%% --------------------------------------
%% ------- callbacks --------------------
%% --------------------------------------
-callback def_kv() ->
    #{Key :: atom() => DefaultValue :: any()}.

-callback ext_key(BinKey :: binary()) ->
    Key :: atom().

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

-callback type(Key :: atom()) ->
    smallint | int2 | integer | int4 |bigint | int8 |
    real | float | float4 | float8 |
    string | text | varchar | binary |
    date | datetime |
    boolean | bool |
    list | {list, integer}.

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
        Data <- data(Module),
        {Module, Data, Meta}
    ]).

data(Module) ->
    DefData = Module:def_kv(),
    Fun = fun(Key,Value, {AccData, Errors}) ->
        case Value of
            undefined ->
                NewData = maps:put(Key, Value, AccData),
                {NewData, Errors};
            _ ->
                %% check default values
                case codd_typecast:typecast(Module, Key, Value) of
                    {ok, TypecastValue} ->
                        NewData = maps:put(Key, TypecastValue, AccData),
                        {NewData, Errors};
                    {error, Reason} ->
                        {AccData, [{error, Reason} | Errors]}
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
        case is_meta_option(K,V) of
            true ->
                NewAcc = maps:update(K,V,Acc),
                {NewAcc, Error};
            false ->
                {Acc, [codd_error:unvalid_option(K) | Error]}
        end
    end,
    case maps:fold(Fun, {Meta, []}, Opts) of
        {Meta2, []} -> {ok, Meta2};
        {_, Errors} -> {error, Errors}
    end.

is_meta_option(from_db, true) -> true;
is_meta_option(from_db, false) -> true;
is_meta_option(changed_fields, V) when is_map(V) ->  true;
is_meta_option(_, _) -> false.

def_meta()->
    #{
        from_db => false,
        changed_fields => #{}
    }.
changed_fields({_, _, #{changed_fields := CF}}) ->
    CF.
is_from_db({_, _, #{from_db := FDB}}) ->
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
set(Key, Value, {Module, _, _} = Model) ->
    do([error_m ||
        CurValue <- check_key(Key, Model),
        TypeCastValue <- codd_typecast:typecast(Module, Key, Value),
        case TypeCastValue of
            CurValue ->
                return(Model);
            NewValue ->
                Model2 = update_model(Key, NewValue, Model),
                return(Model2)
        end
    ]).

update_model(Key, Value, {Module, Data, #{changed_fields := CF} = Meta}) ->
    Data2 = maps:update(Key, Value, Data),
    Meta2 = maps:put(changed_fields, maps:put(Key, Value, CF), Meta),
    {Module, Data2, Meta2}.

%% ---------------------------
%% from_proplist(List, Model, #{ignore_unknown => true})
%% #{ignore_unknown => true} allow ignore unknown keys
%% ---------------------------
from_proplist(List, Model, #{ignore_unknown := true}) ->
    Fun = fun({Key,Value}, {AccModel, Errors}) ->
               case set(Key, Value, AccModel) of
                   {ok, NewModel} ->
                       {NewModel, Errors};
                   {error, {Key, unknown}} ->
                       {AccModel, Errors};
                   {error, Error} ->
                       {AccModel, [Error | Errors]}
               end
    end,
    case lists:foldl(Fun, {Model, []}, List) of
        {Data2, []} -> {ok, Data2};
        {_, Errors} -> {error, Errors}
    end;
from_proplist(List, Model, _) ->
    Fun = fun({Key,Value}, {AccModel, Errors}) ->
               case set(Key, Value, AccModel) of
                   {ok, NewModel} ->
                       {NewModel, Errors};
                   {error, Error} ->
                       {AccModel, [Error | Errors]}
                end
    end,
    case lists:foldl(Fun, {Model, []}, List) of
        {Data2, []} -> {ok, Data2};
        {_, Errors} -> {error, Errors}
    end.

%% ---------------------------
%% from_ext_proplist(List, Model, #{ignore_unknown => true})
%% ignoring:
%% - undefined bin keys
%% - trying to write to readonly keys
%% - undefined model keys
%% ---------------------------
from_ext_proplist(List, {Module, _, _} = Model, #{ignore_unknown := true}) ->
    Fun = fun({BinKey,Value}, {AccModel, Errors}) ->
        case ext_key(BinKey, Module) of
            {ok, Key} ->
                case Module:is_w(Key) of
                    true ->
                        case set(Key, Value, AccModel) of
                            {ok, NewModel} ->
                                {NewModel, Errors};
                            {error, {Key, unknown}} ->
                                {AccModel, Errors};
                            {error, Error} ->
                                {AccModel, [Error | Errors]}
                        end;
                    false ->
                        {AccModel, Errors}
                end;
            {error, _} ->
                {AccModel, Errors}
        end
    end,
    case lists:foldl(Fun, {Model, []}, List) of
        {Data2, []} -> {ok, Data2};
        {_, Errors} -> {error, Errors}
    end;
from_ext_proplist(List, {Module, _, _} = Model, _) ->
    Fun = fun({BinKey,Value}, {AccModel, Errors}) ->
        case ext_key(BinKey, Module) of
            {ok, Key} ->
                case Module:is_w(Key) of
                    true ->
                        case set(Key, Value, AccModel) of
                            {ok, NewModel} ->
                                {NewModel, Errors};
                            {error, Error} ->
                                {AccModel, [Error | Errors]}
                        end;
                    false ->
                        {AccModel, [codd_error:unknown_error(BinKey) | Errors]}
                end;
            {error, Error} ->
                {AccModel, [Error | Errors]}
        end
    end,
    case lists:foldl(Fun, {Model, []}, List) of
        {Data2, []} -> {ok, Data2};
        {_, Errors} -> {error, Errors}
    end.

from_db(List, {Module, Data, Meta}) ->
    Fun = fun({Key,Value}, Acc) ->
        maps:update(Key, Value, Acc)
    end,
    Data2 = lists:foldl(Fun, Data, List),
    {ok, {Module, Data2, Meta}}.

from_map(Map, Model, Opts) ->
    List = maps:to_list(Map),
    from_proplist(List, Opts, Model).

from_ext_map(Map, Model, Opts) ->
    List = maps:to_list(Map),
    from_ext_proplist(List, Opts, Model).

value(Key, {_, Data, _}) ->
    maps:get(Key, Data).

fields(Keys, {_, Data, _}) when is_list(Keys) ->
    maps:with(Keys, Data);
fields(Key, Model) ->
    fields([Key], Model).

to_ext_map(Model) ->
    to_map(is_r, Model).

to_map({_, Data, _})->
    Data.
to_map(Flag, {Module, Data, _}) ->
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

to_proplist({_, Data, _}) ->
    maps:to_list(Data).

db_keys({Module, Data, _Meta}) ->
    [atom_to_binary(X, latin1) || X <- maps:keys(Data), Module:is_db(X)];
db_keys(Module) ->
    Data = Module:def_kv(),
    [atom_to_binary(X, latin1) || X <- maps:keys(Data), Module:is_db(X)].
db_keys(Keys, {Module, Data, _Meta}) ->
    Data2 = maps:with(Keys, Data),
    [atom_to_binary(X, latin1) || X <- maps:keys(Data2), Module:is_db(X)];
db_keys(Keys, Module) ->
    Data = Module:def_kv(),
    Data2 = maps:with(Keys, Data),
    [atom_to_binary(X, latin1) || X <- maps:keys(Data2), Module:is_db(X)].

check_key(Key, {_, Data, _}) ->
    case maps:find(Key, Data) of
        {ok, Value} ->
            {ok, Value};
        error ->
            {error, codd_error:unknown_error(Key)}
    end.

ext_key(BinKey, Module) ->
    try
        Key = Module:ext_key(BinKey),
        {ok, Key}
    catch
        error:_  ->
            {error, codd_error:unknown_error(BinKey)}
    end.


