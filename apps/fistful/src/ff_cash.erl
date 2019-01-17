-module(ff_cash).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

-export([decode/1]).
-export([encode/1]).

-spec decode(dmsl_domain_thrift:'Cash'()) -> ff_transaction:body().
decode(#domain_Cash{amount = Amount, currency = Currency}) ->
    {Amount, Currency#domain_CurrencyRef.symbolic_code}.

-spec encode(ff_transaction:body()) -> dmsl_domain_thrift:'Cash'().
encode({Amount, CurrencyID}) ->
    #domain_Cash{
        amount = Amount,
        currency = #domain_CurrencyRef{symbolic_code = CurrencyID}
    }.
