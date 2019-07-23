%%% Withdrawal adapter generic
-module(ff_adapter).

%%
%% Types
%%

-type adapter() :: ff_woody_client:client().
-type state()   :: %% as stolen from `machinery_msgpack`
    nil                |
    boolean()          |
    integer()          |
    float()            |
    binary()           | %% string
    {binary, binary()} | %% binary
    [state()]     |
    #{state() => state()}.

-type opts() :: #{binary() => binary()}.

-export_type([adapter/0]).
-export_type([state/0]).
-export_type([opts/0]).
