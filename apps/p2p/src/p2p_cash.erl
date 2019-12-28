-module(p2p_cash).

-include_lib("damsel/include/dmsl_p2p_adapter_thrift.hrl").

-export([to_dmsl/1]).
-export([from_domain/1]).

-export_type([t/0]).
-type t() :: #{
    amount   := ff_cash:amount(),
    currency := ff_currency:currency()
}.

-spec to_dmsl(t()) ->
    dmsl_p2p_adapter_thrift:'Cash'().
to_dmsl(#{amount := Amount, currency := Currency}) ->
    #p2p_adapter_Cash{
        amount   = Amount,
        currency = ff_dmsl_codec:marshal(currency, Currency)
    }.

-spec from_domain(ff_cash:cash()) ->
    t().
from_domain({Amount, CurrencyID}) ->
    Currency = ff_pipeline:unwrap(currency, ff_currency:get(CurrencyID)),
    #{
        amount   => Amount,
        currency => Currency
    }.
