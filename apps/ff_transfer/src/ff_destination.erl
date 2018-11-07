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
-type status()   :: ff_identity:status().
-type resource() ::
    {bank_card, resource_bank_card()}.

-type resource_bank_card() :: #{
    token          := binary(),
    payment_system => atom(), % TODO
    bin            => binary(),
    masked_pan     => binary()
}.

-type destination() :: ff_instrument:instrument(resource()).
-type params()      :: ff_instrument_machine:params(resource()).
-type machine()     :: ff_instrument_machine:st(resource()).
-type event()       :: ff_instrument:event(resource()).

-export_type([id/0]).
-export_type([destination/0]).
-export_type([status/0]).
-export_type([resource/0]).
-export_type([event/0]).

%% Accessors

-export([account/1]).
-export([id/1]).
-export([name/1]).
-export([identity/1]).
-export([currency/1]).
-export([resource/1]).
-export([status/1]).

%% API

-export([create/3]).
-export([get_machine/1]).
-export([get/1]).
-export([is_accessible/1]).

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

-spec is_accessible(destination()) ->
    {ok, accessible} |
    {error, ff_party:inaccessibility()}.

is_accessible(Destination) ->
    ff_instrument:is_accessible(Destination).
