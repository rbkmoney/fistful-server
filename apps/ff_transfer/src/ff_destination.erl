%%%
%%% Destination
%%%
%%% TODOs
%%%
%%%  - We must consider withdrawal provider terms ensure that the provided
%%%    Resource is ok to withdraw to.
%%%

-module(ff_destination).

-type ctx()      :: ff_ctx:ctx().
-type id()       :: ff_instrument:id().
-type name()     :: ff_instrument:name().
-type account()  :: ff_account:account().
-type identity() :: ff_identity:id().
-type currency() :: ff_currency:id().
-type status()   :: ff_instrument:status().

-type resource_type() :: bank_card | crypto_wallet.
-type resource() ::
    {bank_card, resource_bank_card()} |
    {crypto_wallet, resource_crypto_wallet()}.

-type resource_extension() :: #{
    type := resource_type(),
    id   := resource_data_id(),
    data := resource_extension_data()
}.

-type resource_data_id() ::
    {bank_card, resource_bank_card_data_id()} |
    {crypto_wallet, #{}}.

-type resource_bank_card_data_id() :: #{
    bin_data_id    := ff_bin_data:bin_data_id()
}.

-type resource_extension_data() ::
    {bank_card, resource_bank_card_extension()} |
    {crypto_wallet, #{}}.

-type resource_bank_card_extension() :: #{
    bank_name           => binary(),
    iso_country_code    => binary(),
    card_type           => charge_card | credit | debit | credit_or_debit
}.

-type resource_bank_card() :: #{
    token          := binary(),
    payment_system => atom(), % TODO
    bin            => binary(),
    masked_pan     => binary()
}.

-type resource_crypto_wallet() :: #{
    id       := binary(),
    currency := atom(),
    tag      => binary()
}.

-type destination() :: ff_instrument:instrument(resource()).
-type params()      :: ff_instrument_machine:params(resource()).
-type machine()     :: ff_instrument_machine:st(resource()).
-type event()       :: ff_instrument:event(resource()).

-type events()  :: ff_instrument_machine:events(resource()).

-export_type([id/0]).
-export_type([destination/0]).
-export_type([status/0]).
-export_type([resource/0]).
-export_type([resource_type/0]).
-export_type([resource_extension/0]).
-export_type([event/0]).
-export_type([params/0]).

%% Accessors

-export([account/1]).
-export([id/1]).
-export([name/1]).
-export([identity/1]).
-export([currency/1]).
-export([resource/1]).
-export([status/1]).
-export([external_id/1]).

%% API

-export([create/3]).
-export([get_machine/1]).
-export([get/1]).
-export([ctx/1]).
-export([is_accessible/1]).
-export([events/2]).
-export([extend_resource/2]).
-export([get_resource_extension/1]).

%% Pipeline

-import(ff_pipeline, [unwrap/2]).

%% Accessors

-spec id(destination())       -> id().
-spec name(destination())     -> name().
-spec account(destination())  -> account().
-spec identity(destination()) -> identity().
-spec currency(destination()) -> currency().
-spec resource(destination()) -> resource().
-spec status(destination())   -> status().


id(Destination)       -> ff_instrument:id(Destination).
name(Destination)     -> ff_instrument:name(Destination).
identity(Destination) -> ff_instrument:identity(Destination).
currency(Destination) -> ff_instrument:currency(Destination).
resource(Destination) -> ff_instrument:resource(Destination).
status(Destination)   -> ff_instrument:status(Destination).
account(Destination)  -> ff_instrument:account(Destination).

-spec external_id(destination()) ->
    id() | undefined.

external_id(T)        -> ff_instrument:external_id(T).

%% API

-define(NS, 'ff/destination_v2').

-spec create(id(), params(), ctx()) ->
    ok |
    {error,
        _InstrumentCreateError |
        exists
    }.

create(ID, Params, Ctx) ->
    ff_instrument_machine:create(?NS, ID, Params, Ctx).

-spec get_machine(id()) ->
    {ok, machine()}       |
    {error, notfound} .

get_machine(ID) ->
    ff_instrument_machine:get(?NS, ID).

-spec get(machine()) ->
    destination().

get(Machine) ->
    ff_instrument_machine:instrument(Machine).

-spec ctx(machine()) ->
    ctx().

ctx(St) ->
    ff_machine:ctx(St).

-spec is_accessible(destination()) ->
    {ok, accessible} |
    {error, ff_party:inaccessibility()}.

is_accessible(Destination) ->
    ff_instrument:is_accessible(Destination).

-spec events(id(), machinery:range()) ->
    {ok, events()} |
    {error, notfound}.

events(ID, Range) ->
    ff_instrument_machine:events(?NS, ID, Range).

-spec extend_resource(resource_extension() | undefined, destination()) ->
    {ok, {resource_type(), map()}} | {error, {resource_type, _Type, _AnoterType}}.

extend_resource(#{type := bank_card, data := {bank_card, Data}}, Destination) ->
    case resource(Destination) of
        {bank_card, Resource} ->
            {ok, {bank_card, maps:merge(Resource, Data)}};
        {Type, _} ->
            {error, {resource_type, bank_card, Type}}
    end;
extend_resource(#{type := crypto_wallet, data := {crypto_wallet, Data}}, Destination) ->
    case resource(Destination) of
        {crypto_wallet, Resource} ->
            {ok, {crypto_wallet, maps:merge(Resource, Data)}};
        {Type, _} ->
            {error, {resource_type, crypto_wallet, Type}}
    end;
extend_resource(undefined, Destination) ->
    {ok, resource(Destination)}.

-spec get_resource_extension(destination()) ->
    {ok, resource_extension()} |
    {error,
        {bin_data, not_found}
    }.

get_resource_extension(Destination) ->
    case resource(Destination) of
        {bank_card, #{token := Token}} ->
            BinData = unwrap(bin_data, ff_bin_data:get(Token)),
            KeyList = [bank_name, iso_country_code, card_type],
            {ok, #{
                type => bank_card,
                id   => {bank_card, #{bin_data_id => ff_bin_data:id(BinData)}},
                data => {bank_card, maps:with(KeyList, BinData)}
            }};
        {crypto_wallet, _CryptoWallet} ->
            {ok, #{
                type => crypto_wallet,
                id   => {crypto_wallet, #{}},
                data => {crypto_wallet, #{}}
            }}
    end.
