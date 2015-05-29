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
            {error, codd_error:unvalid_error(Key, Value)}
    end.

typecast(_, null) ->
    {ok, null};

typecast(Type, Arg)  when
    Type =:= smallint;
    Type =:= int2;
    Type =:= integer;
    Type =:= int4;
    Type =:= bigint;
    Type =:= int8
    ->
    case Arg of
        Bin when is_binary(Bin) ->
            try
                {ok, binary_to_integer(Bin)}
            catch _:_ ->
                {error, bad_arg}
            end;
        Int when is_integer(Int) ->
            {ok, Int};
        _ ->
            {error, bad_arg}
    end;

typecast(Type, Arg) when
    Type =:= real;
    Type =:= float;
    Type =:= float4;
    Type =:= float8 ->
    case Arg of
        Bin when is_binary(Bin) ->
            try
                {ok, binary_to_integer(Bin)}
            catch _:_ ->
                try
                    {ok, binary_to_float(Bin)}
                catch _:_ ->
                    {error, bad_arg}
                end
            end;
        Int when is_integer(Int) ->
            {ok, Int};
        Float when is_float(Float) ->
            {ok, Float};
        _ ->
            {error, bad_arg}
    end;
typecast(Type, Arg) when
    Type =:= string;
    Type =:= text;
    Type =:= varchar;
    Type =:= binary ->
    typecast({binary, #{}}, Arg);
typecast({binary, Opts}, Arg) ->
    case Arg of
        Bin when is_binary(Bin) ->
            case maps:find(max_size, Opts) of
                {ok, MaxSize} ->
                    case byte_size(Arg) < MaxSize of
                        true -> {ok, Bin};
                        false -> {error, bad_arg}
                    end;
                error -> {ok, Bin}
            end;
        _ ->
            {error, bad_arg}
    end;
typecast(date, Arg) ->
    case Arg of
        {_Y, _M, _D} ->
            case calendar:valid_date(Arg) of
                true ->
                    {ok, Arg};
                false ->
                    {error, bad_arg}
            end;
        _ ->
            {error, bad_arg}
    end;
typecast(datetime, Arg) ->
    case Arg of
        {Date, {Hour, Min, Sec}} when
            Hour < 24 andalso Hour >= 0 andalso
            Min < 60 andalso Min >= 0 andalso
            Sec < 60 andalso Sec >= 0
            ->
                case calendar:valid_date(Date) of
                    true ->
                        {ok, Arg};
                    false ->
                        {error, bad_arg}
                end;
        _ ->
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
typecast(list, _) ->
    {error, bad_arg};

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
    end.

