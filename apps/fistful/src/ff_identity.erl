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

-type provider_id() :: ff_provider:id().
-type party_id()    :: ff_party:id().
-type contract_id() :: ff_party:contract_id().

-type identity() :: #{
    party        := party_id(),
    provider     := provider_id(),
    class        := class_id(),
    contract     => contract_id()
}.

-type prototype() :: #{
    provider := provider_id(),
    class    := class_id()
}.

-export_type([prototype/0]).
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

-export([is_accessible/1]).

-export([create/2]).
-export([set_contract/2]).

%%

-export([contract_template/1]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/2]).

%% Accessors

-spec provider(identity()) -> provider_id().
provider(#{provider := V}) -> V.

-spec class(identity()) -> class_id().
class(#{class := V})    -> V.

-spec party(identity()) -> party_id().
party(#{party := V})    -> V.

-spec contract(identity()) ->
    {ok, ff_contract:id()} |
    {error, notfound}.

contract(#{contract := ContractID}) ->
    {ok, ContractID};
contract(#{}) ->
    {error, notfound}.

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

-spec create(party_id(), prototype()) ->
    {ok, identity()} |
    {error,
        {party, {inaccessible, blocked | suspended}} |
        {provider, notfound}                          |
        {identity_class, notfound}
    }.

create(PartyID, #{
    provider := ProviderID,
    class    := ClassID
}) ->
    do(fun () ->
        accessible = unwrap(party, ff_party:is_accessible(PartyID)),
        Provider   = unwrap(provider, ff_provider:get(ProviderID)),
        _Class     = unwrap(identity_class, ff_provider:get_identity_class(ClassID, Provider)),
        #{
            party    => PartyID,
            provider => ProviderID,
            class    => ClassID
        }
    end).

-spec set_contract(ff_contract:id(), identity()) ->
    {ok, identity()} |
    {error, exists}.

set_contract(_ContractID, #{contract := _}) ->
    {error, exists};
set_contract(ContractID, Identity = #{}) ->
    {ok, Identity#{contract => ContractID}}.
