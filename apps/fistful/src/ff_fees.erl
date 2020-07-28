-module(ff_fees).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([surplus/1]).
-export([unmarshal/1]).
-export([compute/2]).

-export_type([plan/0]).
-export_type([final/0]).
-export_type([computation_error/0]).

-type plan() :: fees(cash_volume()).
-type final() :: fees(cash()).

-type fees(T) :: #{fees := #{cash_flow_constant() => T}}.

-type cash_flow_constant() :: ff_cash_flow:plan_constant().
-type cash_volume() :: ff_cash_flow:plan_volume().
-type cash() :: ff_cash:cash().

-type computation_error() :: {cash_flow_constant(), ff_cash_flow:volume_finalize_error()}.

-import(ff_pipeline, [do/1, unwrap/2]).

-spec surplus(plan() | final()) ->
    cash_volume() | cash() | undefined.
surplus(#{fees := Fees}) ->
    maps:get(surplus, Fees, undefined).

-spec unmarshal(dmsl_domain_thrift:'Fees'()) ->
    plan().
unmarshal(#domain_Fees{fees = Fees}) ->
    DecodedFees = maps:map(
        fun(_Key, Value) ->
            ff_cash_flow:decode_domain_plan_volume(Value)
        end,
        Fees
    ),
    #{fees => DecodedFees}.

-spec compute(plan(), cash()) ->
    {ok, final()} | {error, computation_error()}.
compute(#{fees := Fees}, Cash) ->
    Constants = #{operation_amount => Cash},
    do(fun() ->
        ComputedFees = maps:map(
            fun(CashFlowConstant, CashVolume) ->
                unwrap(CashFlowConstant, ff_cash_flow:compute_volume(CashVolume, Constants))
            end,
            Fees
        ),
        #{fees => ComputedFees}
    end).
