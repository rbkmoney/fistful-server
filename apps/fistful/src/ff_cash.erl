-module(ff_cash).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

-export([decode/1]).
-export([encode/1]).
-export([validate_cash_to_cash/2]).

-spec decode(dmsl_domain_thrift:'Cash'()) -> ff_transaction:body().
decode(#domain_Cash{amount = Amount, currency = Currency}) ->
    {Amount, Currency#domain_CurrencyRef.symbolic_code}.

-spec encode(ff_transaction:body()) -> dmsl_domain_thrift:'Cash'().
encode({Amount, CurrencyID}) ->
    #domain_Cash{
        amount = Amount,
        currency = #domain_CurrencyRef{symbolic_code = CurrencyID}
    }.

-spec validate_cash_to_cash(ff_transaction:body(), ff_transaction:body()) ->
    valid | {error, invalid_currency | invalid_amount}.
validate_cash_to_cash({Amount1, _}, {Amount2, _}) when
    Amount1 < 0 orelse
    Amount1 > Amount2
->
    {error, invalid_amount};
validate_cash_to_cash({_, Currency1}, {_, Currency2}) when
    Currency1 =/= Currency2
->
    {error, invalid_currency};
validate_cash_to_cash(_, _) ->
    valid.
