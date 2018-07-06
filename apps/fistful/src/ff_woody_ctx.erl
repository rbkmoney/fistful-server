%%%
%%% Woody context helpers
%%%

-module(ff_woody_ctx).

-export([set/1]).
-export([get/0]).
-export([unset/0]).

-export([get_user_identity/0]).

%%

-type context() :: woody_context:ctx().

-spec set(context()) ->
    ok.

set(Context) ->
    true = gproc:reg({p, l, ?MODULE}, Context),
    ok.

-spec get() ->
    context().

get() ->
    gproc:get_value({p, l, ?MODULE}).

-spec unset() ->
    ok.

unset() ->
    true = gproc:unreg({p, l, ?MODULE}),
    ok.

%%

-spec get_user_identity() ->
    woody_user_identity:user_identity().

get_user_identity() ->
    woody_user_identity:get(?MODULE:get()).
