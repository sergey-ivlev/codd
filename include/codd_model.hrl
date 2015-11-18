%%%-------------------------------------------------------------------
%%% @author isergey
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Mar 2015 11:57 AM
%%%-------------------------------------------------------------------
-author("isergey").
-behaviour(codd_model).

-export([new/0, new/1, to_proplist/1, to_ext_proplist/1, to_ext_map/1, to_map/1, to_map/2]).
-export([is_from_db/1]).

-export([from_proplist/1,       from_proplist/2,        from_proplist/3]).
-export([from_ext_proplist/1,   from_ext_proplist/2,    from_ext_proplist/3]).
-export([from_map/1,            from_map/2,             from_map/3]).
-export([from_ext_map/1,        from_ext_map/2,         from_ext_map/3]).
-export([from_db/1,             from_db/2]).

-export([db_table/1, driver/1]).

-export([set/3, value/2, fields/2]).
-export([is_changed/2]).


%% @doc make new mpdel
new() ->
    codd_model:new(?MODULE).
new(Opts) ->
    codd_model:new(?MODULE, Opts).

%% --------------------------------------
%% ------- set api ----------------------
%% --------------------------------------
set(Key, Value, Model) ->
    codd_model:set(Key, Value, Model).

is_changed(Key, Model) ->
    codd_model:is_changed(Key, Model).
is_from_db(Model) ->
    codd_model:is_from_db(Model).
%% --------------------------------------
%% @doc Opts usage
%% Opts is a map object
%% by default, all opts are disabled
%% example of active options
%% #{
%%    ignore_unknown => true
%% }
%% all values, different from atom true, disabled option
%% @end
%% --------------------------------------
from_proplist(List) ->
    from_proplist(List, new()).
from_proplist(List, {?MODULE, Data, Meta}) ->
    from_proplist(List, {?MODULE, Data, Meta}, #{});
from_proplist(List, Opts) ->
    from_proplist(List, new(), Opts).
from_proplist(List, Model, Opts) ->
    codd_model:from_proplist(List, Model, Opts).

from_ext_proplist(List) ->
    from_ext_proplist(List, new()).
from_ext_proplist(List, {?MODULE, Data, Meta}) ->
    from_ext_proplist(List, {?MODULE, Data, Meta}, #{});
from_ext_proplist(List, Opts) ->
    from_ext_proplist(List, new(), Opts).
from_ext_proplist(List, Model, Opts) ->
    codd_model:from_ext_proplist(List, Model, Opts).


from_map(ExtMap) ->
    from_map(ExtMap, new()).
from_map(ExtMap, {?MODULE, Data, Meta}) ->
    from_map(ExtMap, {?MODULE, Data, Meta}, #{});
from_map(ExtMap, Opts) ->
    from_map(ExtMap, new(), Opts).
from_map(ExtMap, Model, Opts) ->
    codd_model:from_map(ExtMap, Model, Opts).

from_ext_map(ExtMap) ->
    from_ext_map(ExtMap, new()).
from_ext_map(ExtMap, {?MODULE, Data, Meta}) ->
    from_ext_map(ExtMap, {?MODULE, Data, Meta}, #{});
from_ext_map(ExtMap, Opts) ->
    from_ext_map(ExtMap, new(), Opts).
from_ext_map(ExtMap, Model, Opts) ->
    codd_model:from_ext_map(ExtMap, Model, Opts).


from_db(DBPropList) ->
    from_db(DBPropList, new(#{from_db => true})).
from_db(DBPropList, Model) ->
    codd_model:from_db(DBPropList, Model).


%% --------------------------------------
%% ------- get api ----------------------
%% --------------------------------------
value(Key, Model) ->
    codd_model:value(Key, Model).
fields(Keys, Model) ->
    codd_model:fields(Keys, Model).

to_ext_map(Model) ->
    codd_model:to_ext_map(Model).

to_map(Model) ->
    codd_model:to_map(Model).
to_map(Flag, Model) ->
    codd_model:to_map(Flag, Model).

to_ext_proplist(Model) ->
    codd_model:to_ext_proplist(Model).

to_proplist(Model) ->
    codd_model:to_proplist(Model).

%% --------------------------------------
%% --------------- DB -------------------
%% --------------------------------------
db_table({?MODULE, _Data, _Meta}) ->
    ?MODULE:db_table().
driver({?MODULE, _Data, _Meta}) ->
    ?MODULE:driver().
