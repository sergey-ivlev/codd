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
-export([is_primary/1, is_r/1, is_w/1, is_db/1, is_required/1, is_prevent_select/1]).
-export([bin_to_key/1]).
-export([type/1]).

-export([db_table/0, driver/0]).

-export([get/1, save/1, save/2, delete/1, find/1]).

-include_lib("codd/include/model_core.hrl").
%%% -----------------------------
%%% --------- KEYS --------------
%%% -----------------------------
def_kv() -> #{
    id => undefined
}.

bin_to_key(<<"id">>) -> id.

% all field are from db
is_db(_) -> true.

% all field autogenerate select
is_prevent_select(_) -> false.

%% ---- db primary key ------
is_primary(id) -> true;
is_primary(_) -> false.

%% --- user read only key ---
is_r(id) -> true;
is_r(_) -> false.

%% ---- user write key ------
is_w(_) -> false.

is_required(advertiser_id) -> true;
is_required(_) -> false.

%%% -----------------------------
%%% ----- KEY MANIPULATION-------
%%% -----------------------------
% default type is integer
type(_) -> integer.

%%% -----------------------------
%%% ----- MODEL INFO-------------
%%% -----------------------------
%% use db_table() for default get/save/find/delete generator
db_table() -> <<"change_db_table_name_here">>.
driver() -> codd_postgres.

%%% -----------------------------
%%% ---------- API --------------
%%% -----------------------------

get(Id) ->
    codd:get(?MODULE, #{id => Id}).

find(Conditions) ->
    codd:find(?MODULE, Conditions).

save(Model) ->
    codd:save(Model).
save(Connection, Model) ->
    codd:save(Connection, Model).

delete(Model) ->
    codd:delete(Model).
