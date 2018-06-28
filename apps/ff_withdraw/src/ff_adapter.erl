%%% Withdrawal adapter generic
-module(ff_adapter).

%%
%% Types
%%

-type adapter() :: ff_woody_client:client().
-type state() :: any().
-type opts() :: #{binary() => binary()}.

-export_type([adapter/0]).
-export_type([state/0]).
-export_type([opts/0]).
