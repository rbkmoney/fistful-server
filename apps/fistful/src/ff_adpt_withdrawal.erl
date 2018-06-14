-module(ff_adpt_withdrawal).

%%
%% Types
%%

-type destination() :: ff_destination:destination().
-type identity() :: ff_identity:identity().
-type cash() :: ff_transfer:body().

-type withdrawal() :: #{
    id => binary(),
    destination => destination(),
    cash => cash(),
    sender => identity() | undefined,
    receiver => identity() | undefined
}.

-export_type([destination/0]).
-export_type([identity/0]).
-export_type([cash/0]).
-export_type([withdrawal/0]).
