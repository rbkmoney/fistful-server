%%%
%%% Internal client context
%%%

-module(ff_ctx).

-type ctx() :: #{namespace() => metadata()}.

-type namespace() :: binary().
-type metadata()  :: machinery_msgpack:t().

-export_type([ctx/0]).

-export([new/0]).

%%

-spec new() ->
    ctx().

new() ->
    #{}.
