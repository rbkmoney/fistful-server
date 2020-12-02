%%% Withdrawal adapter generic
-module(ff_adapter).

%%
%% Types
%%

-type adapter() :: ff_woody_client:client().
%% as stolen from `machinery_msgpack`
-type state() ::
    nil
    | boolean()
    | integer()
    | float()
    %% string
    | binary()
    %% binary
    | {binary, binary()}
    | [state()]
    | #{state() => state()}.

-type opts() :: #{binary() => binary()}.

-type transaction_info() :: #{
    id := binary(),
    timestamp => binary(),
    extra := #{binary() => binary()},
    additional_info => additional_transaction_info()
}.

-type additional_transaction_info() :: #{
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
    three_ds_verification => three_ds_verification()
}.

-type three_ds_verification() ::
    authentication_successful
    | attempts_processing_performed
    | authentication_failed
    | authentication_could_not_be_performed.

-type failure() :: ff_failure:failure().

-export_type([adapter/0]).
-export_type([state/0]).
-export_type([opts/0]).
-export_type([transaction_info/0]).
-export_type([failure/0]).
