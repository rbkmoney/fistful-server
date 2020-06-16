%%%
%%% P2P template model
%%%

-module(p2p_template).

%% API

-export([create/1]).
-export([set_blocking/2]).
-export([force_create_transfer/2]).
-export([create_transfer/2]).

%% Accessors

-export([id/1]).
-export([blocking/1]).
-export([party_revision/1]).
-export([domain_revision/1]).
-export([created_at/1]).
-export([identity_id/1]).
-export([details/1]).
-export([external_id/1]).
-export([template_body/1]).
-export([template_metadata/1]).

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
    details := details(),
    blocking => blocking(),
    external_id => id()
}.

-opaque template() :: #{
    version := ?ACTUAL_FORMAT_VERSION,
    id := id(),
    identity_id := identity_id(),
    domain_revision := domain_revision(),
    party_revision := party_revision(),
    created_at := timestamp(),
    details := details(),
    external_id => id()
}.

-type blocking() :: unblocked | blocked.

-type details() :: #{
    body := template_body(),
    metadata => template_metadata()
}.

-type template_body() :: #{
    value := body()
}.

-type template_metadata() :: #{
    value := metadata()
}.

-type event() ::
    {created, template()} |
    {blocking_changed, blocking()}.

-type amount() :: integer().
-type body() :: #{
    amount => amount(),
    currency := ff_currency:id()
}.

-type template_cash() :: {amount() | undefined, ff_currency:id()}.

-type metadata() :: ff_entity_context:md().
-type timestamp() :: ff_time:timestamp_ms().

-type identity_id() :: ff_identity:id().
-type identity() :: ff_identity:identity().

-type params() :: #{
    id := id(),
    identity_id := identity_id(),
    details := details(),
    external_id => id()
}.

-type create_error() ::
    {identity, notfound} |
    {terms, ff_party:validate_p2p_template_creation_error()}.

-type transfer_params() :: #{
    id := id(),
    body := body(),
    sender := participant(),
    receiver := participant(),
    context := ctx(),
    quote => quote(),
    client_info => p2p_transfer:client_info(),
    deadline => deadline(),
    metadata => metadata()
}.

-export_type([event/0]).
-export_type([params/0]).
-export_type([template/0]).
-export_type([template_state/0]).
-export_type([blocking/0]).
-export_type([create_error/0]).
-export_type([template_cash/0]).
-export_type([transfer_params/0]).

%%
%% Internal types
%%

-type id() :: machinery:id().

-type legacy_event() :: any().

-type party_revision() :: ff_party:revision().
-type domain_revision() :: ff_domain_config:revision().
-type process_result() :: {undefined, [event()]}.
-type quote() :: p2p_quote:quote().
-type participant() :: p2p_participant:participant().
-type deadline() :: p2p_session:deadline().
-type ctx() :: ff_entity_context:context().

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Accessors

-spec id(template_state()) ->
    id().

id(#{id := V}) ->
    V.

-spec identity_id(template_state()) ->
    identity_id().

identity_id(#{identity_id := V}) ->
    V.

-spec blocking(template_state()) ->
    blocking() | undefined.

blocking(T) ->
    maps:get(blocking, T, undefined).

-spec details(template_state()) ->
    details().

details(#{details := V}) ->
    V.

-spec party_revision(template_state()) -> party_revision().
party_revision(#{party_revision := PartyRevision}) ->
    PartyRevision.

-spec domain_revision(template_state()) -> domain_revision().
domain_revision(#{domain_revision := DomainRevision}) ->
    DomainRevision.

-spec created_at(template_state()) ->
    timestamp().

created_at(#{created_at := V}) ->
    V.

-spec external_id(template_state()) ->
    id() | undefined.

external_id(T) ->
    maps:get(external_id, T, undefined).

-spec template_body(template_state()) ->
    template_cash().

template_body(#{details := #{body := #{value := Body}}}) ->
    template_body_to_cash(Body).

-spec template_metadata(template_state()) ->
    metadata() | undefined.

template_metadata(#{details := V}) ->
    case maps:get(metadata, V, undefined) of
        undefined ->
            undefined;
        #{value := Meatadata} ->
            Meatadata
    end.

%% API

-spec create(params()) ->
    {ok, [event()]} |
    {error, create_error()}.
create(Params = #{
    id := ID,
    identity_id := IdentityID,
    details := Details = #{body := #{value := Body}}
}) ->
    do(fun() ->
        Identity = unwrap(identity, get_identity(IdentityID)),
        {ok, PartyRevision} = ff_party:get_revision(ff_identity:party(Identity)),
        PartyID = ff_identity:party(Identity),
        ContractID = ff_identity:contract(Identity),
        CreatedAt = ff_time:now(),
        DomainRevision = ff_domain_config:head(),
        Varset = create_party_varset(Details),
        {ok, Terms} = ff_party:get_contract_terms(
            PartyID, ContractID, Varset, CreatedAt, PartyRevision, DomainRevision
        ),
        valid =  unwrap(terms, ff_party:validate_p2p_template_creation(Terms, template_body_to_cash(Body))),
        Template = genlib_map:compact(#{
            version => ?ACTUAL_FORMAT_VERSION,
            id => ID,
            identity_id => IdentityID,
            domain_revision => DomainRevision,
            party_revision => PartyRevision,
            details => Details,
            created_at => CreatedAt,
            external_id => maps:get(external_id, Params, undefined)
        }),
        [{created, Template}, {blocking_changed, unblocked}]
    end).

-spec set_blocking(blocking(), template_state()) ->
    {ok, process_result()}.
set_blocking(Blocking, #{blocking := Blocking}) ->
    {ok, {undefined, []}};
set_blocking(Blocking, _State) ->
    {ok, {undefined, [{blocking_changed, Blocking}]}}.

-spec force_create_transfer(id(), transfer_params()) ->
    ok | {error, p2p_transfer:create_error() | exists}.
force_create_transfer(ID, Params) ->
    do(fun() ->
        Machine = unwrap(p2p_template_machine:get(ID)),
        State = p2p_template_machine:p2p_template(Machine),
        {Result, _} = create_transfer(Params, State),
        unwrap(Result)
    end).

-spec create_transfer(transfer_params(), template_state()) ->
    {ok | {error, p2p_transfer:create_error() | exists}, process_result()}.
create_transfer(Params = #{
    id := ID,
    body := Body,
    sender := Sender,
    receiver := Receiver,
    context := Context
}, Template) ->
    Quote = maps:get(quote, Params, undefined),
    ClientInfo = maps:get(client_info, Params, undefined),
    Deadline = maps:get(deadline, Params, undefined),
    TransferMeta = maps:get(metadata, Params, undefined),
    CreateTransferParams = genlib_map:compact(#{
        id => ID,
        identity_id => identity_id(Template),
        body => Body,
        sender => Sender,
        receiver => Receiver,
        quote => Quote,
        client_info => ClientInfo,
        deadline => Deadline,
        metadata => merge_metadata(TransferMeta, template_metadata(Template))
    }),
    Result = p2p_transfer_machine:create(
        CreateTransferParams,
        Context
    ),
    {Result, {undefined, []}}.

merge_metadata(undefined, undefined) ->
    undefined;
merge_metadata(undefined, TemplateMeta) ->
    TemplateMeta;
merge_metadata(TransferMeta, undefined) ->
    TransferMeta;
merge_metadata(TransferMeta, TemplateMeta) ->
    maps:merge(TransferMeta, TemplateMeta).

create_party_varset(#{body := #{value := Body}}) ->
    {Amount, Currency} = template_body_to_cash(Body),
    case Amount of
        undefined ->
            #{
                currency => ff_dmsl_codec:marshal(currency_ref, Currency)
            };
        Amount ->
            #{
                currency => ff_dmsl_codec:marshal(currency_ref, Currency),
                cost => ff_dmsl_codec:marshal(cash, {Amount, Currency})
            }
    end.

template_body_to_cash(Body = #{currency := Currency}) ->
    Amount = maps:get(amount, Body, undefined),
    {Amount, Currency}.

%% P2PTemplate validators

-spec get_identity(identity_id()) ->
    {ok, identity()} | {error, notfound}.
get_identity(IdentityID) ->
    do(fun() ->
        IdentityMachine = unwrap(ff_identity_machine:get(IdentityID)),
        ff_identity_machine:identity(IdentityMachine)
    end).

%% Events apply

-spec apply_event(event(), undefined | template_state()) ->
    template_state().

apply_event({created, Template}, undefined) ->
    Template;
apply_event({blocking_changed, Blocking}, Template) ->
    Template#{blocking => Blocking}.

-spec maybe_migrate(event() | legacy_event(), ff_machine:migrate_params()) ->
    event().

% Other events
maybe_migrate(Ev, _MigrateParams) ->
    Ev.
