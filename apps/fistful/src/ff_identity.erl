%%%
%%% Identity
%%%
%%% Essentially a contract + a number of identity claims.
%%%  * What Payment Institution? Why does it matter?
%%%
%%% We should know:
%%%  * What are the fees?
%%%  * What are the limits?
%%%  * Who will sell us e-money? This is a party + shop pair probably.
%%%  * Who will provide us withdrawals? This is a party + shop pair probably.
%%%

-module(ff_identity).

%% API

-type party()       :: ff_party:id().
-type provider()    :: ff_provider:provider().
-type contract()    :: ff_party:contract().

-type identity() :: #{
    party        := party(),
    provider     := provider(),
    class        := class(),
    contract     => contract()
}.

-type ev() ::
    {contract_set, contract()}.

-type outcome() ::
    [ev()].

-export_type([identity/0]).
-export_type([ev/0]).

%% TODO
%%  - Factor out into dedicated module

-type class_id()              :: binary().
-type contract_template_ref() :: dmsl_domain_thrift:'ContractTemplateRef'().

-type class() :: #{
    contract_template_ref := contract_template_ref(),
    initial_level_id      := level_id(),
    levels                := #{level_id() => level()},
    challenges            := #{challenge_id() => challenge()}
}.

-type level_id()         :: binary().
-type contractor_level() :: dmsl_domain_thrift:'ContractorIdentificationLevel'().

-type level() :: #{
    name             := binary(),
    contractor_level := contractor_level()
}.

-type challenge_id() :: binary().

-type challenge() :: #{
    name             := binary(),
    base_level_id    := level_id(),
    target_level_id  := level_id()
}.

-export_type([class_id/0]).
-export_type([class/0]).
-export_type([level_id/0]).
-export_type([level/0]).
-export_type([challenge_id/0]).
-export_type([challenge/0]).

-export([provider/1]).
-export([party/1]).
-export([class/1]).
-export([contract/1]).

-export([is_accessible/1]).

-export([create/3]).
-export([setup_contract/1]).
-export([start_challenge/2]).

-export([apply_event/2]).

%%

-export([contract_template/1]).
-export([initial_level/1]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%% Accessors

-spec provider(identity()) -> provider().
provider(#{provider := V}) -> V.

-spec class(identity()) -> class().
class(#{class := V})    -> V.

-spec party(identity()) -> party().
party(#{party := V})    -> V.

-spec contract(identity()) ->
    {ok, contract()} |
    {error, notfound}.

contract(V) ->
    ff_map:find(contract, V).

-spec is_accessible(identity()) ->
    {ok, accessible} |
    {error, {inaccessible, suspended | blocked}}.

is_accessible(Identity) ->
    ff_party:is_accessible(party(Identity)).

%% Class

-spec contract_template(class()) -> contract_template_ref().
contract_template(#{contract_template_ref := V}) -> V.

-spec initial_level(class()) ->
    level().

initial_level(#{initial_level_id := V} = Identity) ->
    {ok, Level} = level(V, Identity),
    Level.

-spec level(level_id(), class()) ->
    {ok, level()} |
    {error, notfound}.

level(ID, #{levels := Levels}) ->
    ff_map:find(ID, Levels).

%% Level

-spec contractor_level(level()) -> contractor_level().
contractor_level(#{contractor_level := V}) -> V.

%% Constructor

-spec create(party(), provider(), class()) ->
    {ok, identity()}.

create(Party, Provider, Class) ->
    do(fun () ->
        #{
            party    => Party,
            provider => Provider,
            class    => Class
        }
    end).

-spec setup_contract(identity()) ->
    {ok, outcome()} |
    {error,
        invalid
    }.

setup_contract(Identity) ->
    do(fun () ->
        Class    = class(Identity),
        Contract = unwrap(ff_party:create_contract(party(Identity), #{
            payinst           => ff_provider:payinst(provider(Identity)),
            contract_template => contract_template(Class),
            contractor_level  => contractor_level(initial_level(Class))
        })),
        [{contract_set, Contract}]
    end).

-spec start_challenge(level(), identity()) ->
    {ok, outcome()} |
    {error,
        {level, invalid}
    }.

start_challenge(Level, Identity) ->
    oops.

%%

-spec apply_event(ev(), identity()) ->
    identity().

apply_event({contract_set, C}, Identity) ->
    Identity#{contract => C}.
