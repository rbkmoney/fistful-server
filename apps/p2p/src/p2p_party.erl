-module(p2p_party).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-type varset() :: hg_selector:varset().
-type varset_params() :: #{
    cash        := ff_cash:cash(),
    party_id    := ff_party:id(),
    sender      := p2p_instrument:instrument(),
    receiver    := p2p_instrument:instrument()
}.

-export([create_varset/1]).

-spec create_varset(varset_params()) ->
    varset().
create_varset(#{cash := Cash} = Params) ->
    {_, Currency} = Cash,
    #{
        party_id => maps:get(party_id, Params),
        currency => encode_currency(Currency),
        cost     => encode_cash(Cash),
        p2p_tool => encode_p2p_tool(maps:get(sender, Params), maps:get(receiver, Params))
    }.

encode_cash({Amount, Currency}) ->
    #domain_Cash{
        amount   = Amount,
        currency = encode_currency(Currency)
    }.

encode_currency(Currency) ->
    #domain_CurrencyRef{symbolic_code = Currency}.

encode_p2p_tool(Sender, Receiver) ->
    #domain_P2PTool{
        sender = p2p_instrument:construct_payment_tool(Sender),
        receiver = p2p_instrument:construct_payment_tool(Receiver)
    }.
