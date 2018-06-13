%%%
%%% Account
%%%
%%% Holds financial assets.
%%%

-module(ff_account).

%%

-type id()       :: dmsl_accounter_thrift:'AccountID'().
-type currency() :: ff_currency:id().
-type account()  :: {id(), currency()}.

-export_type([id/0]).

-export([create/2]).

-spec create(currency(), ff_woody_client:caller()) ->
    account().

create(Currency, WoodyCaller) ->
    {42, Currency}.
