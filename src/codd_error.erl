%%%-------------------------------------------------------------------
%%% @author isergey
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Apr 2015 6:14 AM
%%%-------------------------------------------------------------------
-module(codd_error).
-author("isergey").

%% API
-export([unknown_option/1, unvalid_option/1]).
-export([unknown_error/1, unvalid_error/1, required_error/1]).

unknown_option(K) -> {K, option_unknown}.
unvalid_option(K) -> {K, not_valid}.

unknown_error(K) -> {K, unknown}.
unvalid_error(K) -> {K, not_valid}.
required_error(K) -> {K, required}.
