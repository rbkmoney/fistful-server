%%% Withdrawal adapter generic
-module(ff_adapter).

%%
%% Types
%%

-type adapter() :: ff_woody_client:client().
-type state() :: any().

-export_type([adapter/0]).
-export_type([state/0]).
