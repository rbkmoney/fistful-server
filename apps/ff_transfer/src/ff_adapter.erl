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

-type transaction_info() :: #{
    id              := binary(),
    extra           := #{binary() => binary()},
    timestamp       => binary(),
    additional_info => additional_trx_info()
}.

-type additional_trx_info()         :: #{
    rrn                   => binary(),
    approval_code         => binary(),
    acs_url               => binary(),
    pareq                 => binary(),
    md                    => binary(),
    term_url              => binary(),
    pares                 => binary(),
    eci                   => binary(),
    cavv                  => binary(),
    xid                   => binary(),
    cavv_algorithm        => binary(),
    three_ds_verification => binary()
}.

-export_type([adapter/0]).
-export_type([state/0]).
-export_type([opts/0]).
-export_type([transaction_info/0]).
