%%%
%%% Withdrawal
%%%

-module(ff_withdrawal).

-type body()       :: {integer(), ff_currency:id()}.

-type withdrawal() :: #{
    source      := ff_wallet_machine:id(),
    destination := ff_destination_machine:id(),
    body        := body()
}.


%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%%

create(SourceID, DestinationID, Body = {_, Currency}) ->
    do(fun () ->
        Source      = unwrap(source, ff_wallet_machine:get(SourceID)),
        accessible  = unwrap(source, ff_wallet:is_accessible(Source)),
        Currency    = unwrap(currency, is_matching(Currency, ff_wallet:currency(Source))),
        Destination = unwrap(destination, ff_destination_machine:get(DestinationID)),
        DestWallet  = ff_destination:wallet(Source),
        Currency    = unwrap(currency, is_matching(Currency, ff_wallet:currency(DestWallet))),
        accessible  = unwrap(destination, ff_wallet:is_accessible(DestWallet)),
        ProviderID  = unwrap(provider, ff_withdrawal_provider:choose(Source, Destination, Body))
    end).

is_matching(Currency, Currency) ->
    {ok, Currency};
is_matching(_, _) ->
    {error, invalid}.
