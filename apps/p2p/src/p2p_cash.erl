-module(p2p_cash).

-export([from_domain/1]).

-export_type([t/0]).
-type t() :: #{
    amount   := ff_cash:amount(),
    currency := ff_currency:currency()
}.

-spec from_domain(ff_cash:cash()) ->
    t().
from_domain({Amount, CurrencyID}) ->
    Currency = ff_pipeline:unwrap(currency, ff_currency:get(CurrencyID)),
    #{
        amount   => Amount,
        currency => Currency
    }.
