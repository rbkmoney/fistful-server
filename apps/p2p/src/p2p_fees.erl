-module(p2p_fees).

-include_lib("damsel/include/dmsl_p2p_adapter_thrift.hrl").

-export([to_dmsl/1]).
-export([from_domain_computed/1]).

-export_type([t/0]).
-type t() :: #{fees := #{cash_flow_constant() => cash()}}.

-type cash_flow_constant() :: ff_cash_flow:plan_constant().
-type cash() :: p2p_cash:t().

-spec to_dmsl(t()) ->
    dmsl_p2p_adapter_thrift:'Fees'().
to_dmsl(#{fees := Fees}) ->
    #p2p_adapter_Fees{
        fees = maps:map(
            fun(_CashFlowConstant, Cash) ->
                p2p_cash:to_dmsl(Cash)
            end,
            Fees
        )
    }.

-spec from_domain_computed(ff_fees:computed()) ->
    t().
from_domain_computed(#{fees := DomainFees}) ->
    #{fees => maps:map(
        fun(_CashFlowConstant, Cash) ->
            % FIXME: do we need to use domain_revision to get currency?
            p2p_cash:from_domain(Cash)
        end,
        DomainFees
    )}.
