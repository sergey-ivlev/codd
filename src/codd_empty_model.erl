%%%-------------------------------------------------------------------
%%% @author isergey
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Apr 2015 7:04 AM
%%%-------------------------------------------------------------------
-module(codd_empty_model).
-author("isergey").

-export([def_kv/0]).
-export([is_primary/1, is_ro/1, is_rw/1, is_db/1, is_required/1]).
-export([is_valid/2]).
-export([bin_to_key/1]).
-export([type/1]).

-export([db_table/0, driver/0]).
-export([alias/1]).

-export([get/1, save/1, save/2, delete/1, find/1]).

-include_lib("codd/include/model_core.hrl").
%%% -----------------------------
%%% --------- KEYS --------------
%%% -----------------------------
def_kv() -> #{
    id => undefined
}.

bin_to_key(<<"id">>) -> id.

% in this model alias is not used
alias(_) -> no.

% all field are from db
is_db(_) -> true.

%% ---- db primary key ------
is_primary(id) -> true;
is_primary(_) -> false.

%% --- user read only key ---
is_ro(id) -> true;
is_ro(_) -> false.

%% ---- user write key ------
is_rw(_) -> false.

is_required(advertiser_id) -> true;
is_required(_) -> false.

%%% -----------------------------
%%% ----- KEY MANIPULATION-------
%%% -----------------------------
is_valid(id, Val) when is_integer(Val) -> true;
is_valid(_, null) -> true;
is_valid(_, _) -> false.

% default type is integer
type(_) -> integer.

%%% -----------------------------
%%% ----- MODEL INFO-------------
%%% -----------------------------
%% use db_table() for default get/save/find/delete generator
db_table() -> <<"change_db_table_name_here">>.
driver() -> codd_pg_driver.

%%% -----------------------------
%%% ---------- API --------------
%%% -----------------------------

get(Id) ->
    codd_model:get(?MODULE, #{id => Id}).

find(Conditions) ->
    codd_model:find(?MODULE, Conditions).

save(Model) ->
    codd_model:save(Model).
save(Connection, Model) ->
    codd_model:save(Connection, Model).

delete(Model) ->
    codd_model:delete(Model).
