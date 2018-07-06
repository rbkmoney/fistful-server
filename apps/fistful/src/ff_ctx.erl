%%%
%%% Internal client context
%%%

-module(ff_ctx).

-type ctx() :: #{namespace() => md()}.

-type namespace() :: binary().
-type md()        :: %% as stolen from `machinery_msgpack`
    nil                |
    boolean()          |
    integer()          |
    float()            |
    binary()           | %% string
    {binary, binary()} | %% binary
    [md()]             |
    #{md() => md()}    .

-export_type([ctx/0]).

-export([new/0]).
-export([get/2]).

%%

-spec new() ->
    ctx().

new() ->
    #{}.

-spec get(namespace(), ctx()) ->
    {ok, md()}       |
    {error, notfound}.

get(Ns, Ctx) ->
    ff_map:find(Ns, Ctx).
