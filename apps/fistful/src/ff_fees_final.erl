-module(ff_fees_final).

-export([surplus/1]).

-export_type([fees/0]).

-type fees() :: #{fees := #{cash_flow_constant() => cash()}}.

-type cash_flow_constant() :: ff_cash_flow:plan_constant().
-type cash() :: ff_cash:cash().

-spec surplus(fees()) -> cash() | undefined.
surplus(#{fees := Fees}) ->
    maps:get(surplus, Fees, undefined).
