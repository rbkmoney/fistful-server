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

-export_type([identity/0]).

%% TODO
%%  - Factor out into dedicated module
-type class_id()              :: binary().
-type contract_template_ref() :: dmsl_domain_thrift:'ContractTemplateRef'().

-type class()    :: #{
    contract_template_ref := contract_template_ref()
}.

-export_type([class_id/0]).
-export_type([class/0]).

-export([provider/1]).
-export([party/1]).
-export([class/1]).
-export([contract/1]).

-export([set_contract/2]).

-export([is_accessible/1]).

-export([create/3]).
-export([setup_contract/1]).

%%

-export([contract_template/1]).

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

-spec set_contract(contract(), identity()) ->
    identity().

set_contract(ContractID, Identity = #{}) ->
    Identity#{contract => ContractID}.

-spec is_accessible(identity()) ->
    {ok, accessible} |
    {error, {inaccessible, suspended | blocked}}.

is_accessible(Identity) ->
    ff_party:is_accessible(party(Identity)).

%%

-spec contract_template(class()) ->
    _ContractTemplateRef.

contract_template(#{contract_template_ref := V}) ->
    V.

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
    {ok, identity()} |
    {error,
        {inaccessible, blocked | suspended} |
        invalid
    }.

setup_contract(Identity) ->
    do(fun () ->
        accessible = unwrap(is_accessible(Identity)),
        Contract   = unwrap(ff_party:create_contract(party(Identity), #{
            payinst           => ff_provider:payinst(provider(Identity)),
            contract_template => contract_template(class(Identity))
        })),
        ff_identity:set_contract(Contract, Identity)
    end).
