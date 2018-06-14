%%% Withdrawal generic
-module(ff_wthadpt).

%%
%% Types
%%

-type adapter() :: atom().
-type wth_id() :: binary().
-type destination() :: dmsl_withdrawals_provider_adapter_thrift:'Destination'().
-type identity() :: dmsl_withdrawals_provider_adapter_thrift:'Identity'().
-type cash() :: dmsl_withdrawals_provider_adapter_thrift:'Cash'().
-type failure() :: dmsl_domain_thrift:'Failure'().
-type adapter_state() :: any().

-type withdrawal() :: #{
    id => wth_id(),
    body => cash(),
    destination => destination(),
    sender => identity() | indefined,
    receiver => identity() | indefined
}.

-export_type([adapter/0]).
-export_type([wth_id/0]).
-export_type([destination/0]).
-export_type([identity/0]).
-export_type([cash/0]).
-export_type([failure/0]).
-export_type([withdrawal/0]).
-export_type([adapter_state/0]).
