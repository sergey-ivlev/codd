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


-export([db_keys/1, db_keys/2]).


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

-callback is_prevent_select(Key :: atom()) ->
    PSFlag :: boolean().

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
                        case codd_typecast:typecast(Module, Key, SourceValue) of
                            {ok, _} ->
                                NewData = maps:put(Key, AliasValue, AccData),
                                {NewData, Errors};
                            {error, Reason} ->
                                {AccData, [{error, Reason} | Errors]}
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
set(Key, Value, {Module, _Meta, _Data} = Model) ->
    do([error_m ||
        CurValue <- check_key(Key, Model),
        {FindedValue, FindedAliasValue} <- codd_model:find_alias(Module, Key, Value),
        _TypeCastValue <- codd_typecast:typecast(Module, Key, FindedValue),
        case FindedAliasValue of
            CurValue ->
                return(Model);
            NewValue ->
                Model2 = update_model(Key, NewValue, Model),
                return(Model2)
        end
    ]).

update_model(Key, Value, {Module, #{changed_fields := CF} = Meta, Data}) ->
    Data2 = maps:update(Key, Value, Data),
    Meta2 = maps:put(changed_fields, maps:put(Key, Value, CF), Meta),
    {Module, Meta2, Data2}.

from_proplist(List, Opts, Model) ->
    Fun = fun({Key,Value}, {AccModel, Errors}) ->
               case set(Key, Value, AccModel) of
                   {ok, NewModel} ->
                       {NewModel, Errors};
                   {error, {Key, unknown}} ->
                        case maps:find(ignore_unknown, Opts) of
                            {ok, true} ->
                                {AccModel, Errors};
                            _ ->
                                {AccModel, [{Key, unknown} | Errors]}
                        end;
                   {error, Error} ->
                       {AccModel, [Error | Errors]}
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
                                {AccModel, [codd_error:unknown_error(BinKey) | Errors]}
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
    List = maps:to_list(Map),
    from_proplist(List, Opts, Model).

from_ext_map(Map, Opts, Model) ->
    List = maps:to_list(Map),
    from_ext_proplist(List, Opts, Model).

from_db(DBPropList, _Opts, {Module, Meta, Data}) ->
    Fun = fun({BinKey,Value}, {AccModel, Errors}) ->
        case bin_to_key(Module, BinKey) of
            {ok, Key} ->
                case Module:is_db(Key) of
                    true ->
                        case codd_typecast:typecast(Module, Key, Value) of
                            {ok, TypeCastVal} ->
                                case find_alias(Module, Key, TypeCastVal) of
                                    {ok, {_, AliasValue}} ->
                                        RoundVal = round_datetime(AliasValue),
                                        AccModel2 = maps:update(Key, RoundVal, AccModel),
                                        {AccModel2, Errors};
                                    {error, Reason} ->
                                        {AccModel, [Reason | Errors]}
                                end;
                            {error, Reason} ->
                                {AccModel, [Reason | Errors]}
                        end;
                    false ->
                        {AccModel, [codd_error:unknown_error(BinKey) | Errors]}
                end;
            {error, Reason} ->
                {AccModel, [Reason | Errors]}
        end
    end,
    case lists:foldl(Fun, {Data, []}, DBPropList) of
        {Data2, []} -> {ok, {Module, Meta, Data2}};
        {_, Errors} -> {error, Errors}
    end.

%% fix postgres ISO 8601 datatime storing with floating seconds
round_datetime({{Y,M,D},{H,Min,S}}) ->
    {{Y,M,D},{H,Min,round(S)}};
round_datetime(Data) -> Data.

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

db_keys({Module, _Meta, Data}) ->
    [atom_to_binary(X, latin1) || X <- maps:keys(Data), Module:is_db(X), not Module:is_prevent_select(X)];
db_keys(Module) ->
    Data = Module:def_kv(),
    [atom_to_binary(X, latin1) || X <- maps:keys(Data), Module:is_db(X), not Module:is_prevent_select(X)].
db_keys(Keys, {Module, _Meta, Data}) ->
    Data2 = maps:with(Keys, Data),
    [atom_to_binary(X, latin1) || X <- maps:keys(Data2), Module:is_db(X), not Module:is_prevent_select(X)];
db_keys(Keys, Module) ->
    Data = Module:def_kv(),
    Data2 = maps:with(Keys, Data),
    [atom_to_binary(X, latin1) || X <- maps:keys(Data2), Module:is_db(X), not Module:is_prevent_select(X)].

without_alias({Module, #{changed_fields := CF}, Data}) ->
    do([error_m ||
        NewCF <- without_alias(Module, CF),
        NewData <- without_alias(Module, Data),
        {Module, #{changed_fields => NewCF}, NewData}
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

check_key(Key, {_, _, Data}) ->
    case maps:find(Key, Data) of
        {ok, Value} ->
            {ok, Value};
        error ->
            {error, codd_error:unknown_error(Key)}
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
                    {error, codd_error:alias_error(Key)};
                [AliasPair | _] ->
                    {ok, AliasPair}
            end;
        AliasKey when is_atom(AliasKey) ->
            find_alias(Module, AliasKey, Value);
        _ ->
            {error, codd_error:alias_error(Key)}
    end.

bin_to_key(Module, BinKey) ->
    try
        Key = Module:bin_to_key(BinKey),
        {ok, Key}
    catch
        error:_  ->
            {error, codd_error:unknown_error(BinKey)}
    end.


