%%%
%%% P2P template model
%%%

-module(p2p_template).

%% API

-export([create/1]).

%% Accessors

-export([id/1]).
-export([blocking/1]).
-export([party_revision/1]).
-export([domain_revision/1]).
-export([created_at/1]).
-export([identity_id/1]).
-export([fields/1]).
-export([external_id/1]).

%% ff_machine
-export([apply_event/2]).
-export([maybe_migrate/2]).

%%
%% Types
%%
-define(ACTUAL_FORMAT_VERSION, 1).

-opaque template_state() :: #{
    id := id(),
    identity_id := identity_id(),
    domain_revision := domain_revision(),
    party_revision := party_revision(),
    created_at := timestamp(),
    blocking := blocking(),
    fields := fields(),
    external_id => id()
}.

-opaque template() :: #{
    version := ?ACTUAL_FORMAT_VERSION,
    id := id(),
    identity_id := identity_id(),
    domain_revision := domain_revision(),
    party_revision := party_revision(),
    created_at := timestamp(),
    blocking := blocking(),
    fields := fields(),
    external_id => id()
}.

-type blocking() :: unblocked | blocked.

-type fields() :: #{
    body => field_body(),
    metadata => field_metadata()
}.

-type field_body() :: #{
    value := body()
}.

-type field_metadata() :: #{
    default := metadata()
}.

-type event() ::
    {created, template()}.

-type body() :: ff_transaction:body().
-type metadata() :: ff_entity_context:md().
-type timestamp() :: ff_time:timestamp_ms().

-type identity_id() :: ff_identity:id().
-type identity() :: ff_identity:identity().

-type params() :: #{
    id := id(),
    identity_id := identity_id(),
    fields := fields(),
    external_id => id()
}.

-type create_error() ::
    {identity, notfound} |
    {terms, ff_party:validate_w2w_transfer_creation_error()}.

-export_type([event/0]).
-export_type([params/0]).
-export_type([template/0]).
-export_type([template_state/0]).

%%
%% Internal types
%%

-type id() :: machinery:id().

-type legacy_event() :: any().

-type party_revision() :: ff_party:revision().
-type domain_revision() :: ff_domain_config:revision().
-type terms() :: ff_party:terms().

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Accessors

-spec id(template()) ->
    id().

id(#{id := V}) ->
    V.

-spec identity_id(template()) ->
    identity_id().

identity_id(#{identity_id := V}) ->
    V.


-spec blocking(template()) ->
    blocking().

blocking(#{blocking := V}) ->
    V.

-spec fields(template()) ->
    fields().

fields(#{fields := V}) ->
    V.

-spec party_revision(template()) -> party_revision().
party_revision(#{party_revision := PartyRevision}) ->
    PartyRevision.

-spec domain_revision(template()) -> domain_revision().
domain_revision(#{domain_revision := DomainRevision}) ->
    DomainRevision.

-spec created_at(template()) ->
    timestamp().

created_at(#{created_at := V}) ->
    V.

-spec external_id(template()) ->
    id() | undefined.

external_id(T) ->
    maps:get(external_id, T, undefined).

%% API

-spec create(params()) ->
    {ok, [event()]} |
    {error, create_error()}.
create(Params = #{
    id := ID,
    identity_id := IdentityID,
    fields := Fields
}) ->
    do(fun() ->
        Identity = unwrap(identity, get_identity(IdentityID)),
        {ok, PartyRevision} = ff_party:get_revision(ff_identity:party(Identity)),
        PartyID = ff_identity:party(Identity),
        ContractID = ff_identity:contract(Identity),
        CreatedAt = ff_time:now(),
        DomainRevision = ff_domain_config:head(),
        Varset = create_party_varset(Fields),
        {ok, Terms} = ff_party:get_contract_terms(
            PartyID, ContractID, Varset, CreatedAt, PartyRevision, DomainRevision
        ),
        valid =  unwrap(terms, validate_p2p_template_creation(Terms, Params)),
        Template = genlib_map:compact(#{
            version => ?ACTUAL_FORMAT_VERSION,
            id => ID,
            identity_id => IdentityID,
            domain_revision => DomainRevision,
            party_revision => PartyRevision,
            fields => Fields,
            blocking => unblocked,
            created_at => CreatedAt,
            external_id => maps:get(external_id, Params, undefined)
        }),
        [{created, Template}]
    end).

create_party_varset(#{body := #{value := Body}}) ->
    {_Amount, Currency} = Body,
    genlib_map:compact(#{
        currency => ff_dmsl_codec:marshal(currency_ref, Currency),
        cost => ff_dmsl_codec:marshal(cash, Body)
    });
create_party_varset(_) ->
    #{}.

%% P2PTemplate validators

-spec validate_p2p_template_creation(terms(), fields()) ->
    {ok, valid} |
    {error, create_error()}.
validate_p2p_template_creation(Terms, #{body := #{value := Body}}) ->
    do(fun() ->
        valid = unwrap(ff_party:validate_p2p_template_creation(Terms, Body))
    end);
validate_p2p_template_creation(Terms, _) ->
    do(fun() ->
        valid = unwrap(ff_party:validate_p2p_template_creation(Terms))
    end).

-spec get_identity(identity_id()) ->
    {ok, identity()} | {error, notfound}.
get_identity(IdentityID) ->
    do(fun() ->
        IdentityMachine = unwrap(ff_identity_machine:get(IdentityID)),
        ff_identity_machine:identity(IdentityMachine)
    end).

%% Events apply

-spec apply_event(event(), undefined | template()) ->
    template().

apply_event({created, Template}, undefined) ->
    Template.

-spec maybe_migrate(event() | legacy_event(), ff_machine:migrate_params()) ->
    event().

% Other events
maybe_migrate(Ev, _MigrateParams) ->
    Ev.
