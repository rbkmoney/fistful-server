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

-type trx_info() :: #{
    id := binary(),
    timestamp => binary(),
    extra := #{binary() => binary()},
    additional_info => additional_trx_info()
}.

-type additional_trx_info()   :: #{
    rrn => binary(),
    approval_code => binary(),
    acs_url => binary(),
    pareq => binary(),
    md => binary(),
    term_url => binary(),
    pares => binary(),
    eci => binary(),
    cavv => binary(),
    xid => binary(),
    cavv_algorithm => binary(),
    three_ds_verification => binary()
}.

-type failure() :: ff_failure:failure().

-export_type([adapter/0]).
-export_type([state/0]).
-export_type([opts/0]).
-export_type([trx_info/0]).
-export_type([failure/0]).
