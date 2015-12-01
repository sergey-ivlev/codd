%%%-------------------------------------------------------------------
%%% @author isergey
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. May 2015 6:44 PM
%%%-------------------------------------------------------------------
-module(codd_typecast).
-author("isergey").

%% API
-export([typecast/3, typecast/2]).

typecast(Module, Key, Value) ->
    Type = Module:type(Key),
    case typecast(Type, Value) of
        {ok, TypeCastValue} ->
            {ok, TypeCastValue};
        {error, bad_arg} ->
            {error, codd_error:unvalid_error(Key)}
    end.

typecast([], Arg) -> {ok, Arg};
typecast([Type|Rest], Arg) ->
    case typecast(Type, Arg) of
        {ok, TypecastValue} ->
            typecast(Rest, TypecastValue);
        {error, bad_arg} ->
            {error, bad_arg}
    end;

typecast(_, null) ->
    {ok, null};

typecast(non_neg_integer, Arg) ->
    case typecast(integer, Arg) of
        {ok, Int} when Int >= 0 ->
            {ok, Int};
        {ok, bad_arg} ->
            {error, bad_arg}
    end;

typecast(Type, Arg)  when
    Type =:= smallint;
    Type =:= int2;
    Type =:= int4;
    Type =:= int8;
    Type =:= integer;
    Type =:= bigint
    ->
    case Arg of
        Int when is_integer(Int) ->
            {ok, Int};
        Bin when is_binary(Bin) ->
            try
                {ok, binary_to_integer(Bin)}
            catch _:_ ->
                {error, bad_arg}
            end;
        _ ->
            {error, bad_arg}
    end;

typecast(Type, Arg) when
    Type =:= real;
    Type =:= float;
    Type =:= float4;
    Type =:= float8 ->
    case Arg of
        Float when is_float(Float) ->
            {ok, Float};
        Int when is_integer(Int) ->
            {ok, Int};
        Bin when is_binary(Bin) ->
            try
                {ok, binary_to_float(Bin)}
            catch _:_ ->
                try
                    {ok, binary_to_integer(Bin)}
                catch _:_ ->
                    {error, bad_arg}
                end
            end;
        _ ->
            {error, bad_arg}
    end;
typecast(Type, Arg) when
    Type =:= string;
    Type =:= text;
    Type =:= varchar;
    Type =:= binary ->
    typecast({binary, #{}}, Arg);
typecast({binary, Opts}, Arg) when is_binary(Arg) ->
    case max_size(Arg, Opts) of
        {ok, Arg} ->
            min_size(Arg, Opts);
        {error, bad_arg} -> {error, bad_arg}
    end;

typecast(date, {_Y, _M, _D} = Arg) ->
    case calendar:valid_date(Arg) of
        true ->
            {ok, Arg};
        false ->
            {error, bad_arg}
    end;
typecast(datetime, {Date, {Hour, Min, Sec}} = Arg) when
        Hour < 24 andalso Hour >= 0 andalso
        Min < 60 andalso Min >= 0 andalso
        Sec < 60 andalso Sec >= 0 ->
    case calendar:valid_date(Date) of
        true ->
            {ok, Arg};
        false ->
            {error, bad_arg}
    end;

typecast(Type, Arg) when
    Type =:= boolean;
    Type =:= bool ->
    case Arg of
        true ->
            {ok, true};
        false ->
            {ok, false};
        _ ->
            {error, bad_arg}
    end;
typecast(list, Arg) when is_list(Arg) ->
    {ok, Arg};

typecast({list, integer}, Arg) ->
    case Arg of
        List when is_list(Arg) ->
            Result = [X || X <- List, is_integer(X) =:= false],
            case Result of
                [] -> {ok, Arg};
                _ ->  {error, bad_arg}
            end;
        _ ->
             {error, bad_arg}
    end;

typecast({custom, Fun}, Arg) ->
    try
        case Fun(Arg) of
            {ok, Value} -> {ok, Value};
            _ ->
                {error, bad_arg}
        end
    catch _:_ ->
        {error, bad_arg}
    end;

typecast(_, _) ->
    {error, bad_arg}.

max_size(Arg, Opts) ->
    case maps:find(max_size, Opts) of
        {ok, MaxSize} ->
            case byte_size(Arg) =< MaxSize of
                true -> {ok, Arg};
                false -> {error, bad_arg}
            end;
        error -> {ok, Arg}
    end.
min_size(Arg, Opts) ->
    case maps:find(min_size, Opts) of
        {ok, MaxSize} ->
            case byte_size(Arg) >= MaxSize of
                true -> {ok, Arg};
                false -> {error, bad_arg}
            end;
        error -> {ok, Arg}
    end.


-include_lib("eunit/include/eunit.hrl").
typecast_integer_test() ->
    Types = [smallint, int2, int4, int8, integer, bigint],
    F = fun(Type) ->
            ?assertEqual({ok, 123}, typecast(Type, 123)),
            ?assertEqual({ok, -123}, typecast(Type, -123)),
            ?assertEqual({ok, 123}, typecast(Type, <<"123">>)),
            ?assertEqual({ok, -123}, typecast(Type, <<"-123">>)),
            ?assertEqual({ok, null}, typecast(Type, null)),
            ?assertEqual({error, bad_arg}, typecast(Type, 1.23)),
            ?assertEqual({error, bad_arg}, typecast(Type, '123')),
            ?assertEqual({error, bad_arg}, typecast(Type, [123])),
            ?assertEqual({error, bad_arg}, typecast(Type, [1,2,3])),
            ?assertEqual({error, bad_arg}, typecast(Type, "123"))
        end,
    lists:map(F, Types),
    ?assertEqual({ok, 123}, typecast(Types, 123)).

typecast_float_test() ->
    Types = [real, float, float4, float8],
    F = fun(Type) ->
            ?assertEqual({ok, 1.23}, typecast(Type, 1.23)),
            ?assertEqual({ok, -1.23}, typecast(Type, -1.23)),
            ?assertEqual({ok, 123}, typecast(Type, 123)),
            ?assertEqual({ok, -123}, typecast(Type, -123)),
            ?assertEqual({ok, 1.23}, typecast(Type, <<"1.23">>)),
            ?assertEqual({ok, -1.23}, typecast(Type, <<"-1.23">>)),
            ?assertEqual({ok, 123}, typecast(Type, <<"123">>)),
            ?assertEqual({ok, -123}, typecast(Type, <<"-123">>)),
            ?assertEqual({ok, null}, typecast(Type, null)),
            ?assertEqual({ok, 1.23}, typecast(Type, 1.23)),
            ?assertEqual({error, bad_arg}, typecast(Type, '1.23')),
            ?assertEqual({error, bad_arg}, typecast(Type, [1.23])),
            ?assertEqual({error, bad_arg}, typecast(Type, [1.2,3])),
            ?assertEqual({error, bad_arg}, typecast(Type, "1.23"))
        end,
    lists:map(F, Types),
    ?assertEqual({ok, 1.23}, typecast(Types, 1.23)).

typecast_binary_test() ->
    Types = [string,text,varchar,binary],
    F = fun(Type) ->
            ?assertEqual({ok, <<"123">>}, typecast(Type, <<"123">>)),
            ?assertEqual({ok, <<"-1.23">>}, typecast(Type, <<"-1.23">>)),
            ?assertEqual({ok, null}, typecast(Type, null)),
            ?assertEqual({error, bad_arg}, typecast(Type, "123")),
            ?assertEqual({error, bad_arg}, typecast(Type, "-1.23")),
            ?assertEqual({error, bad_arg}, typecast(Type, '1.23')),
            ?assertEqual({error, bad_arg}, typecast(Type, [1.23])),
            ?assertEqual({error, bad_arg}, typecast(Type, [1.2,3]))
        end,
    lists:map(F, Types),
    ?assertEqual({ok, <<"123">>}, typecast(Types, <<"123">>)).

typecast_binary_opts_test() ->
    Types = [
        {binary, #{}},
        {binary, #{max_size => 10}},
        {binary, #{min_size => 1}},
        {binary, #{min_size => 1, max_size => 10}}],
    F = fun(Type) ->
            ?assertEqual({ok, <<"123">>}, typecast(Type, <<"123">>)),
            ?assertEqual({ok, <<"-1.23">>}, typecast(Type, <<"-1.23">>)),
            ?assertEqual({ok, null}, typecast(Type, null)),
            ?assertEqual({error, bad_arg}, typecast(Type, "123")),
            ?assertEqual({error, bad_arg}, typecast(Type, "-1.23")),
            ?assertEqual({error, bad_arg}, typecast(Type, '1.23')),
            ?assertEqual({error, bad_arg}, typecast(Type, [1.23])),
            ?assertEqual({error, bad_arg}, typecast(Type, [1.2,3]))
        end,
    lists:map(F, Types),
    ?assertEqual({ok, <<"123">>}, typecast(Types, <<"123">>)).

typecast_binary_opts_wrong_value_test() ->
    Types = [
        {binary, #{max_size => 1}},
        {binary, #{min_size => 10}},
        {binary, #{min_size => 10, max_size => 1}},
        {binary, #{min_size => 1, max_size => 1}},
        {binary, #{min_size => 10, max_size => 10}}
    ],
    F = fun(Type) ->
            ?assertEqual({error, bad_arg}, typecast(Type, <<"123">>)),
            ?assertEqual({error, bad_arg}, typecast(Type, <<"-1.23">>)),
            ?assertEqual({ok, null}, typecast(Type, null)),
            ?assertEqual({error, bad_arg}, typecast(Type, "123")),
            ?assertEqual({error, bad_arg}, typecast(Type, "-1.23")),
            ?assertEqual({error, bad_arg}, typecast(Type, '1.23')),
            ?assertEqual({error, bad_arg}, typecast(Type, [1.23])),
            ?assertEqual({error, bad_arg}, typecast(Type, [1.2,3]))
        end,
    lists:map(F, Types),
    ?assertEqual({error, bad_arg}, typecast(Types, <<"123">>)).
