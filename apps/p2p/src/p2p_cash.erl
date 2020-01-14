-module(p2p_cash).

-include_lib("damsel/include/dmsl_p2p_adapter_thrift.hrl").

-export([marshal/1]).
-export([from_ff_cash/1]).

-export_type([t/0]).
-type t() :: #{
    amount   := ff_cash:amount(),
    currency := ff_currency:currency()
}.

-spec marshal(t()) ->
    dmsl_p2p_adapter_thrift:'Cash'().
marshal(#{amount := Amount, currency := Currency}) ->
    #p2p_adapter_Cash{
        amount   = ff_dmsl_codec:marshal(amount, Amount),
        currency = ff_dmsl_codec:marshal(currency, Currency)
    }.

-spec from_ff_cash(ff_cash:cash()) ->
    t().
from_ff_cash({Amount, CurrencyID} = Cash) ->
    Currency = ff_pipeline:unwrap({currency, Cash}, ff_currency:get(CurrencyID)),
    #{
        amount   => Amount,
        currency => Currency
    }.
