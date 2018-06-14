%%% Withdrawal adapter generic
-module(ff_adpt).

%%
%% Types
%%

-type adapter() :: atom().
-type adapter_state() :: any().

-type withdrawal() :: ff_adpt_withdrawal:withdrawal().

-export_type([adapter/0]).
-export_type([withdrawal/0]).
-export_type([adapter_state/0]).
