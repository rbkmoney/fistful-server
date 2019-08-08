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
-type resource() ::
    {bank_card, resource_bank_card()} |
    {crypto_wallet, resource_crypto_wallet()}.

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
-export_type([event/0]).
-export_type([params/0]).
-export_type([resource_bank_card/0]).

%% Accessors

-export([account/1]).
-export([id/1]).
-export([name/1]).
-export([identity/1]).
-export([currency/1]).
-export([resource/1]).
-export([status/1]).
-export([external_id/1]).
-export([try_get_bank_card_token/1]).

%% API

-export([create/3]).
-export([get_machine/1]).
-export([get/1]).
-export([ctx/1]).
-export([is_accessible/1]).
-export([events/2]).

%% Accessors

-spec id(destination())       -> id().
-spec name(destination())     -> name().
-spec account(destination())  -> account().
-spec identity(destination()) -> identity().
-spec currency(destination()) -> currency().
-spec resource(destination()) -> resource().
-spec status(destination())   -> status().

-spec try_get_bank_card_token(destination()) -> binary() | undefined.

id(Destination)       -> ff_instrument:id(Destination).
name(Destination)     -> ff_instrument:name(Destination).
identity(Destination) -> ff_instrument:identity(Destination).
currency(Destination) -> ff_instrument:currency(Destination).
resource(Destination) -> ff_instrument:resource(Destination).
status(Destination)   -> ff_instrument:status(Destination).
account(Destination)  -> ff_instrument:account(Destination).

try_get_bank_card_token(Destination) ->
    case ff_instrument:resource(Destination) of
        {bank_card, #{token := Token}} ->
            Token;
        _ ->
            undefined
    end.

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
