%%%
%%% Tranfer
%%%

-module(ff_transfer).

-type body()      :: {integer(), ff_currency:id()}.
-type wallet_id() :: ff_wallet_machine:id().

-type status() ::
    created   |
    prepared  |
    committed |
    cancelled .

-type transfer() :: #{
    source      := wallet_id(),
    destination := wallet_id(),
    body        := body(),
    status      := status()
}.

-export_type([body/0]).
-export_type([transfer/0]).
-export_type([status/0]).

-export([create/3]).

% -export([prepare/1]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%%

-spec create(wallet_id(), wallet_id(), body()) ->
    {ok, transfer()} |
    {error,
        {source | destination,
            notfound |
            {inaccessible, blocked | suspended} |
            {currency, invalid} |
            {provider, invalid}
        }
    }.

create(SourceID, DestinationID, Body = {_, Currency}) ->
    do(fun () ->
        Source            = unwrap(source, get_wallet(SourceID, Currency)),
        Destination       = unwrap(destination, get_wallet(DestinationID, Currency)),
        {ok, SrcIdentity} = ff_identity_machine:get(ff_wallet:identity(Source)),
        {ok, DstIdentity} = ff_identity_machine:get(ff_wallet:identity(Destination)),
        Provider          = ff_identity:provider(SrcIdentity),
        valid             = unwrap(destination, do(fun () ->
            unwrap(provider, is_valid(Provider, ff_identity:provider(DstIdentity)))
        end)),
        #{
            source      => SourceID,
            destination => DestinationID,
            body        => Body
        }
    end).

get_wallet(WalletID, Currency) ->
    do(fun () ->
        Wallet     = unwrap(ff_wallet_machine:get(WalletID)),
        accessible = unwrap(ff_wallet:is_accessible(Wallet)),
        valid      = unwrap(currency, is_valid(Currency, ff_wallet:currency(Wallet))),
        Wallet
    end).

is_valid(Currency, Currency) ->
    {ok, valid};
is_valid(_, _) ->
    {error, invalid}.
