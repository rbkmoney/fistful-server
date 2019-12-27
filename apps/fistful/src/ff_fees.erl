-module(ff_fees).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([from_dmsl/1]).
-export([compute/2]).
-export([map/2]).

-export_type([t/0]).
-export_type([computed/0]).

-type t() :: fees(cash_volume()).
-type computed() :: fees(cash()).

-type fees(T) :: #{fees := #{cash_flow_constant() => T}}.

-type cash_flow_constant() :: operation_amount | fees.
-type cash_volume() :: ff_cash_flow:plan_volume().
-type cash() :: ff_cash:cash().

-type map_fun() :: fun((cash_flow_constant(), cash() | cash_volume()) -> any()).

-spec from_dmsl(dmsl_domain_thrift:'Fees'()) ->
    t().
from_dmsl(#domain_Fees{fees = Fees}) ->
    DecodedFees = maps:map(
        fun(_Key, Value) ->
            ff_cash_flow:decode_domain_plan_volume(Value)
        end,
        Fees
    ),
    #{fees => DecodedFees}.

-spec compute(t(), ff_cash:cash()) ->
    computed().
compute(#{fees := Fees}, Cash) ->
    Constants = #{operation_amount => Cash},
    ComputedFees = maps:map(
        fun(_CashFlowConstant, CashVolume) ->
            ff_cash_flow:compute_volume(CashVolume, Constants)
        end,
        Fees
    ),
    #{fees => ComputedFees}.

-spec map(map_fun(), fees(any())) ->
    fees(any()).
map(Fun, #{fees := Fees}) ->
    #{fees => maps:map(Fun, Fees)}.
