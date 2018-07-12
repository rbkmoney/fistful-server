%%%
%%% Fistful machine generic accessors.
%%%

-module(ff_machine).


-type timestamp() :: machinery:timestamp().
-type ctx()       :: ff_ctx:ctx().

-type st() ::#{
    ctx   := ctx(),
    times => {timestamp(), timestamp()},
    _ => _
}.
-export_type([st/0]).

%% Accessors API
-export([ctx/1]).
-export([created/1]).
-export([updated/1]).

%% Accessors

-spec ctx(st())     -> ctx().
-spec created(st()) -> timestamp() | undefined.
-spec updated(st()) -> timestamp() | undefined.

ctx(#{ctx := V}) -> V.
created(St)      -> erlang:element(1, times(St)).
updated(St)      -> erlang:element(2, times(St)).

times(St) ->
    genlib_map:get(times, St, {undefined, undefined}).
