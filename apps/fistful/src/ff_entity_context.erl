%%%
%%% Persistent entity context
%%%

-module(ff_entity_context).

-type context() :: #{namespace() => md()}.

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

-export_type([context/0]).
-export_type([md/0]).

-export([new/0]).
-export([get/2]).

%%

-spec new() ->
    context().
new() ->
    #{}.

-spec get(namespace(), context()) ->
    {ok, md()}       |
    {error, notfound}.
get(Ns, Ctx) ->
    ff_map:find(Ns, Ctx).
