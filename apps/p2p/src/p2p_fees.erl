-module(p2p_fees).

-include_lib("damsel/include/dmsl_p2p_adapter_thrift.hrl").

-export([marshal/1]).
-export([from_ff_fees_final/1]).

-export_type([t/0]).
-type t() :: #{fees := #{cash_flow_constant() => cash()}}.

-type cash_flow_constant() :: ff_cash_flow:plan_constant().
-type cash() :: p2p_cash:t().

-spec marshal(t()) ->
    dmsl_p2p_adapter_thrift:'Fees'().
marshal(#{fees := Fees}) ->
    #p2p_adapter_Fees{
        fees = maps:map(
            fun(_CashFlowConstant, Cash) ->
                p2p_cash:marshal(Cash)
            end,
            Fees
        )
    }.

-spec from_ff_fees_final(ff_fees:final()) ->
    t().
from_ff_fees_final(#{fees := DomainFees}) ->
    #{fees => maps:map(
        fun(_CashFlowConstant, Cash) ->
            % FIXME: do we need to use domain_revision to get currency?
            p2p_cash:from_ff_cash(Cash)
        end,
        DomainFees
    )}.
