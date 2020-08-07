%%%
%%% Source
%%%
%%% TODOs
%%%
%%%  - Implement a generic source instead of a current dummy one.
%%%

-module(ff_source).

-type ctx()      :: ff_entity_context:context().
-type id()       :: ff_instrument:id().
-type name()     :: ff_instrument:name().
-type account()  :: ff_account:account().
-type identity() :: ff_identity:id().
-type currency() :: ff_currency:id().
-type status()   :: ff_instrument:status().
-type resource() :: #{
    type    := internal,
    details => binary()
}.

-type source() :: ff_instrument:instrument(resource()).
-type source_state() :: ff_instrument:instrument_state(resource()).
-type params() :: ff_instrument_machine:params(resource()).
-type machine() :: ff_instrument_machine:st(resource()).
-type event_range() :: ff_instrument_machine:event_range().

-type event() :: ff_instrument:event(resource()).
-type events() :: ff_instrument_machine:events(resource()).

-export_type([id/0]).
-export_type([source/0]).
-export_type([source_state/0]).
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
-export([external_id/1]).

%% API

-export([create/2]).
-export([get_machine/1]).
-export([get_machine/2]).
-export([get/1]).
-export([is_accessible/1]).
-export([events/2]).

%% Accessors

-spec id(source_state())       -> id().
-spec name(source_state())     -> name().
-spec account(source_state())  -> account().
-spec identity(source_state()) -> identity().
-spec currency(source_state()) -> currency().
-spec resource(source_state()) -> resource().
-spec status(source_state())   -> status() | undefined.

id(Source)       -> ff_instrument:id(Source).
name(Source)     -> ff_instrument:name(Source).
identity(Source) -> ff_instrument:identity(Source).
currency(Source) -> ff_instrument:currency(Source).
resource(Source) -> ff_instrument:resource(Source).
status(Source)   -> ff_instrument:status(Source).
account(Source)  -> ff_instrument:account(Source).

-spec external_id(source_state()) ->
    id() | undefined.
external_id(T)   -> ff_instrument:external_id(T).

%% API

-define(NS, 'ff/source_v1').

-spec create(params(), ctx()) ->
    ok |
    {error,
        _InstrumentCreateError |
        exists
    }.

create(Params, Ctx) ->
    ff_instrument_machine:create(?NS, Params, Ctx).

-spec get_machine(id()) ->
    {ok, machine()}       |
    {error, notfound} .
get_machine(ID) ->
    ff_instrument_machine:get(?NS, ID).

-spec get(machine()) ->
    source_state().
get(Machine) ->
    ff_instrument_machine:instrument(Machine).

-spec get_machine(id(), event_range()) ->
    {ok, machine()}       |
    {error, notfound} .

get_machine(ID, EventRange) ->
    ff_instrument_machine:get(?NS, ID, EventRange).

-spec is_accessible(source_state()) ->
    {ok, accessible} |
    {error, ff_party:inaccessibility()}.

is_accessible(Source) ->
    ff_instrument:is_accessible(Source).

-spec events(id(), machinery:range()) ->
    {ok, events()} |
    {error, notfound}.

events(ID, Range) ->
    ff_instrument_machine:events(?NS, ID, Range).
