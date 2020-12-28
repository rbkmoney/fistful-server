%%%
%%% P2PTransfer
%%%

-module(p2p_transfer).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-type id() :: binary().

-define(ACTUAL_FORMAT_VERSION, 3).

-opaque p2p_transfer_state() :: #{
    id := id(),
    body := body(),
    owner := identity_id(),
    created_at := ff_time:timestamp_ms(),
    operation_timestamp := ff_time:timestamp_ms(),
    sender := participant(),
    receiver := participant(),
    domain_revision := party_revision(),
    party_revision := domain_revision(),
    status := status(),

    sender_resource => resource(),
    receiver_resource => resource(),
    client_info => client_info(),
    quote => quote_state(),
    session => session(),
    route => route(),
    risk_score => risk_score(),
    p_transfer => p_transfer(),
    adjustments => adjustments_index(),
    deadline => deadline(),
    external_id => id(),
    metadata => metadata()
}.

-opaque p2p_transfer() :: #{
    version := ?ACTUAL_FORMAT_VERSION,
    id := id(),
    body := body(),
    owner := identity_id(),
    created_at := ff_time:timestamp_ms(),
    operation_timestamp := ff_time:timestamp_ms(),
    sender := participant(),
    receiver := participant(),
    domain_revision := party_revision(),
    party_revision := domain_revision(),
    status := status(),

    client_info => client_info(),
    quote => quote(),
    deadline => deadline(),
    external_id => id(),
    metadata => metadata()
}.

-type params() :: #{
    id := id(),
    identity_id := identity_id(),
    body := body(),
    sender := participant(),
    receiver := participant(),
    quote => quote(),
    client_info => client_info(),
    deadline => deadline(),
    external_id => id(),
    metadata => metadata()
}.

-type quote() :: p2p_quote:quote().

-type quote_state() :: #{
    created_at := ff_time:timestamp_ms(),
    expires_on := ff_time:timestamp_ms(),
    sender := ff_resource:resource_descriptor(),
    receiver := ff_resource:resource_descriptor(),
    expires_on := ff_time:timestamp_ms(),
    fees => ff_fees_final:fees()
}.

-type client_info() :: #{
    ip_address => binary(),
    fingerprint => binary()
}.

-type status() ::
    pending
    | succeeded
    | {failed, failure()}.

-type event() ::
    {created, p2p_transfer()}
    | {resource_got, resource(), resource()}
    | {risk_score_changed, risk_score()}
    | {route_changed, route()}
    | {p_transfer, ff_postings_transfer:event()}
    | {session, session_event()}
    | {status_changed, status()}
    | wrapped_adjustment_event().

-type session_event() :: {session_id(), session_event_payload()}.

-type session_event_payload() ::
    started
    | {finished, session_result()}.

-type resource_owner() :: sender | receiver.

-type create_error() ::
    {identity, notfound}
    | {terms, ff_party:validate_p2p_error()}
    | {resource_owner(), {bin_data, ff_bin_data:bin_data_error()}}
    | {resource_owner(), different_resource}.

-type route() :: #{
    version := 1,
    provider_id := provider_id()
}.

-type adjustment_params() :: #{
    id := adjustment_id(),
    change := adjustment_change(),
    external_id => id()
}.

-type adjustment_change() ::
    {change_status, status()}.

-type start_adjustment_error() ::
    invalid_p2p_transfer_status_error()
    | invalid_status_change_error()
    | {another_adjustment_in_progress, adjustment_id()}
    | ff_adjustment:create_error().

-type unknown_adjustment_error() :: ff_adjustment_utils:unknown_adjustment_error().

-type invalid_status_change_error() ::
    {invalid_status_change, {unavailable_status, status()}}
    | {invalid_status_change, {already_has_status, status()}}.

-type invalid_p2p_transfer_status_error() ::
    {invalid_p2p_transfer_status, status()}.

-type action() :: poll | continue | undefined.

-export_type([p2p_transfer_state/0]).
-export_type([p2p_transfer/0]).
-export_type([id/0]).
-export_type([params/0]).
-export_type([quote/0]).
-export_type([quote_state/0]).
-export_type([event/0]).
-export_type([route/0]).
-export_type([create_error/0]).
-export_type([action/0]).
-export_type([adjustment_params/0]).
-export_type([start_adjustment_error/0]).
-export_type([domain_revision/0]).
-export_type([resource_owner/0]).
-export_type([client_info/0]).

%% Transfer logic callbacks

-export([process_transfer/1]).

%% Accessors

-export([id/1]).
-export([body/1]).
-export([owner/1]).
-export([status/1]).
-export([risk_score/1]).
-export([quote/1]).
-export([route/1]).
-export([external_id/1]).
-export([created_at/1]).
-export([operation_timestamp/1]).
-export([client_info/1]).
-export([party_revision/1]).
-export([domain_revision/1]).
-export([sender/1]).
-export([receiver/1]).
-export([sender_resource/1]).
-export([receiver_resource/1]).
-export([deadline/1]).
-export([metadata/1]).
-export([effective_final_cash_flow/1]).

-export([session_id/1]).
-export([sessions/1]).

%% API

-export([create/1]).
-export([is_finished/1]).

-export([start_adjustment/2]).
-export([find_adjustment/2]).
-export([adjustments/1]).

%% Event source

-export([apply_event/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Internal types
-type body() :: ff_cash:cash().
-type identity() :: ff_identity:identity_state().
-type identity_id() :: ff_identity:id().
-type process_result() :: {action(), [event()]}.
-type final_cash_flow() :: ff_cash_flow:final_cash_flow().
-type external_id() :: id() | undefined.
-type p_transfer() :: ff_postings_transfer:transfer().
-type session_id() :: id().
-type failure() :: ff_failure:failure().
-type session_result() :: p2p_session:session_result().
-type adjustment() :: ff_adjustment:adjustment().
-type adjustment_id() :: ff_adjustment:id().
-type adjustments_index() :: ff_adjustment_utils:index().
-type party_revision() :: ff_party:revision().
-type domain_revision() :: ff_domain_config:revision().
-type party_varset() :: hg_selector:varset().
-type risk_score() :: p2p_inspector:risk_score().
-type participant() :: p2p_participant:participant().
-type resource() :: ff_resource:resource().
-type contract_params() :: p2p_party:contract_params().
-type deadline() :: p2p_session:deadline().
-type metadata() :: ff_entity_context:md().

-type wrapped_adjustment_event() :: ff_adjustment_utils:wrapped_event().

-type provider_id() :: ff_p2p_provider:id().

-type legacy_event() :: any().

-type session() :: #{
    id := session_id(),
    result => session_result()
}.

-type activity() ::
    risk_scoring
    | routing
    | p_transfer_start
    | p_transfer_prepare
    | session_starting
    | session_polling
    | p_transfer_commit
    | p_transfer_cancel
    | {fail, fail_type()}
    | adjustment
    | finish.

-type fail_type() ::
    route_not_found
    | session.

%% Accessors

-spec sender(p2p_transfer_state()) -> participant().
sender(#{sender := Sender}) ->
    Sender.

-spec receiver(p2p_transfer_state()) -> participant().
receiver(#{receiver := Receiver}) ->
    Receiver.

-spec sender_resource(p2p_transfer_state()) -> resource() | undefined.
sender_resource(T) ->
    maps:get(sender_resource, T, undefined).

-spec receiver_resource(p2p_transfer_state()) -> resource() | undefined.
receiver_resource(T) ->
    maps:get(receiver_resource, T, undefined).

%%

-spec quote(p2p_transfer_state()) -> quote_state() | undefined.
quote(T) ->
    maps:get(quote, T, undefined).

-spec id(p2p_transfer_state()) -> id().
id(#{id := V}) ->
    V.

-spec body(p2p_transfer_state()) -> body().
body(#{body := V}) ->
    V.

-spec owner(p2p_transfer_state()) -> identity_id().
owner(#{owner := V}) ->
    V.

-spec status(p2p_transfer_state()) -> status() | undefined.
status(T) ->
    maps:get(status, T, undefined).

-spec risk_score(p2p_transfer_state()) -> risk_score() | undefined.
risk_score(T) ->
    maps:get(risk_score, T, undefined).

-spec route(p2p_transfer_state()) -> route() | undefined.
route(T) ->
    maps:get(route, T, undefined).

-spec external_id(p2p_transfer_state()) -> external_id() | undefined.
external_id(T) ->
    maps:get(external_id, T, undefined).

-spec party_revision(p2p_transfer_state()) -> party_revision().
party_revision(#{party_revision := PartyRevision}) ->
    PartyRevision.

-spec domain_revision(p2p_transfer_state()) -> domain_revision().
domain_revision(#{domain_revision := DomainRevision}) ->
    DomainRevision.

-spec created_at(p2p_transfer_state()) -> ff_time:timestamp_ms().
created_at(T) ->
    maps:get(created_at, T).

-spec operation_timestamp(p2p_transfer_state()) -> ff_time:timestamp_ms().
operation_timestamp(#{operation_timestamp := Timestamp}) ->
    Timestamp.

-spec deadline(p2p_transfer_state()) -> deadline() | undefined.
deadline(T) ->
    maps:get(deadline, T, undefined).

-spec client_info(p2p_transfer_state()) -> client_info() | undefined.
client_info(T) ->
    maps:get(client_info, T, undefined).

-spec metadata(p2p_transfer_state()) -> metadata() | undefined.
metadata(T) ->
    maps:get(metadata, T, undefined).

-spec create_varset(identity(), p2p_transfer_state()) -> p2p_party:varset().
create_varset(Identity, P2PTransferState) ->
    Sender = validate_definition(sender_resource, sender_resource(P2PTransferState)),
    Receiver = validate_definition(receiver_resource, receiver_resource(P2PTransferState)),

    PartyID = ff_identity:party(Identity),
    Params = #{
        party_id => PartyID,
        cash => body(P2PTransferState),
        sender => Sender,
        receiver => Receiver
    },
    p2p_party:create_varset(Params).

-spec merge_contract_params(p2p_quote:quote() | undefined, contract_params()) -> contract_params().
merge_contract_params(undefined, Params) ->
    Params;
merge_contract_params(Quote, Params) ->
    Params#{
        party_revision => p2p_quote:party_revision(Quote),
        domain_revision => p2p_quote:domain_revision(Quote),
        timestamp => p2p_quote:created_at(Quote)
    }.

%% API

-spec create(params()) ->
    {ok, [event()]}
    | {error, create_error()}.
create(TransferParams) ->
    do(fun() ->
        #{
            id := ID,
            body := Body,
            identity_id := IdentityID,
            sender := Sender,
            receiver := Receiver
        } = TransferParams,
        Quote = maps:get(quote, TransferParams, undefined),
        ClientInfo = maps:get(client_info, TransferParams, undefined),
        ExternalID = maps:get(external_id, TransferParams, undefined),
        Deadline = maps:get(deadline, TransferParams, undefined),
        Metadata = maps:get(metadata, TransferParams, undefined),
        CreatedAt = ff_time:now(),
        valid = unwrap(validate_transfer_participants(Sender, Receiver, Quote)),
        SenderResource = unwrap(sender, prepare_resource(sender, Sender, Quote)),
        ReceiverResource = unwrap(receiver, prepare_resource(receiver, Receiver, Quote)),
        Identity = unwrap(identity, get_identity(IdentityID)),
        {ok, PartyRevision0} = ff_party:get_revision(ff_identity:party(Identity)),
        Params = #{
            cash => Body,
            sender => SenderResource,
            receiver => ReceiverResource,
            party_revision => PartyRevision0,
            domain_revision => ff_domain_config:head(),
            timestamp => ff_time:now()
        },
        ContractParams = merge_contract_params(Quote, Params),
        {OperationTimestamp, PartyRevision, DomainRevision, Terms} =
            unwrap(p2p_party:get_contract_terms(Identity, ContractParams)),
        valid = unwrap(terms, ff_party:validate_p2p(Terms, Body)),

        [
            {created,
                genlib_map:compact(#{
                    version => ?ACTUAL_FORMAT_VERSION,
                    id => ID,
                    owner => IdentityID,
                    body => Body,
                    created_at => CreatedAt,
                    operation_timestamp => OperationTimestamp,
                    external_id => ExternalID,
                    sender => Sender,
                    receiver => Receiver,
                    domain_revision => DomainRevision,
                    party_revision => PartyRevision,
                    quote => build_quote_state(Quote),
                    client_info => ClientInfo,
                    status => pending,
                    deadline => Deadline,
                    metadata => Metadata
                })},
            {resource_got, SenderResource, ReceiverResource}
        ]
    end).

validate_transfer_participants(_Sender, _Receiver, undefined) ->
    {ok, valid};
validate_transfer_participants(Sender, Receiver, Quote) ->
    do(fun() ->
        valid = unwrap(sender, validate_transfer_participant(Sender, maps:get(sender, Quote))),
        valid = unwrap(receiver, validate_transfer_participant(Receiver, maps:get(receiver, Quote)))
    end).

validate_transfer_participant(Participant, {bank_card, QuotedParticipant}) ->
    Params = get_participant_resource_params(Participant),
    Token = maps:get(token, maps:get(bank_card, Params)),
    case maps:get(token, QuotedParticipant) of
        Token -> {ok, valid};
        _ -> {error, different_resource}
    end.

get_participant_resource_params({raw, #{resource_params := {bank_card, Params}}}) ->
    Params.

-spec start_adjustment(adjustment_params(), p2p_transfer_state()) ->
    {ok, process_result()}
    | {error, start_adjustment_error()}.
start_adjustment(Params, P2PTransferState) ->
    #{id := AdjustmentID} = Params,
    case find_adjustment(AdjustmentID, P2PTransferState) of
        {error, {unknown_adjustment, _}} ->
            do_start_adjustment(Params, P2PTransferState);
        {ok, _Adjustment} ->
            {ok, {undefined, []}}
    end.

-spec find_adjustment(adjustment_id(), p2p_transfer_state()) ->
    {ok, adjustment()} | {error, unknown_adjustment_error()}.
find_adjustment(AdjustmentID, P2PTransferState) ->
    ff_adjustment_utils:get_by_id(AdjustmentID, adjustments_index(P2PTransferState)).

-spec adjustments(p2p_transfer_state()) -> [adjustment()].
adjustments(P2PTransferState) ->
    ff_adjustment_utils:adjustments(adjustments_index(P2PTransferState)).

%% Сущность в настоящий момент нуждается в передаче ей управления для совершения каких-то действий
-spec is_active(p2p_transfer_state()) -> boolean().
is_active(#{status := succeeded} = P2PTransferState) ->
    is_childs_active(P2PTransferState);
is_active(#{status := {failed, _}} = P2PTransferState) ->
    is_childs_active(P2PTransferState);
is_active(#{status := pending}) ->
    true.

%% Сущность завершила свою основную задачу по переводу денег. Дальше её состояние будет меняться только
%% изменением дочерних сущностей, например запуском adjustment.
-spec is_finished(p2p_transfer_state()) -> boolean().
is_finished(#{status := succeeded}) ->
    true;
is_finished(#{status := {failed, _}}) ->
    true;
is_finished(#{status := pending}) ->
    false.

%% Transfer callbacks

-spec process_transfer(p2p_transfer_state()) -> process_result().
process_transfer(P2PTransferState) ->
    Activity = deduce_activity(P2PTransferState),
    do_process_transfer(Activity, P2PTransferState).

%% Internals

-spec do_start_adjustment(adjustment_params(), p2p_transfer_state()) ->
    {ok, process_result()}
    | {error, start_adjustment_error()}.
do_start_adjustment(Params, P2PTransferState) ->
    do(fun() ->
        valid = unwrap(validate_adjustment_start(Params, P2PTransferState)),
        AdjustmentParams = make_adjustment_params(Params, P2PTransferState),
        #{id := AdjustmentID} = Params,
        {Action, Events} = unwrap(ff_adjustment:create(AdjustmentParams)),
        {Action, ff_adjustment_utils:wrap_events(AdjustmentID, Events)}
    end).

%% Internal getters

-spec prepare_resource(sender | receiver, p2p_participant:participant(), p2p_quote:quote() | undefined) ->
    {ok, resource()}
    | {error, {bin_data, ff_bin_data:bin_data_error()}}.
prepare_resource(sender, Params, undefined) ->
    p2p_participant:get_resource(Params);
prepare_resource(sender, Params, Quote) ->
    p2p_participant:get_resource(Params, p2p_quote:sender_descriptor(Quote));
prepare_resource(receiver, Params, undefined) ->
    p2p_participant:get_resource(Params);
prepare_resource(receiver, Params, Quote) ->
    p2p_participant:get_resource(Params, p2p_quote:receiver_descriptor(Quote)).

-spec p_transfer(p2p_transfer_state()) -> p_transfer() | undefined.
p_transfer(P2PTransferState) ->
    maps:get(p_transfer, P2PTransferState, undefined).

-spec p_transfer_status(p2p_transfer_state()) -> ff_postings_transfer:status() | undefined.
p_transfer_status(P2PTransferState) ->
    case p_transfer(P2PTransferState) of
        undefined ->
            undefined;
        Transfer ->
            ff_postings_transfer:status(Transfer)
    end.

-spec risk_score_status(p2p_transfer_state()) -> unknown | scored.
risk_score_status(P2PTransferState) ->
    case risk_score(P2PTransferState) of
        undefined ->
            unknown;
        _Known ->
            scored
    end.

-spec route_selection_status(p2p_transfer_state()) -> unknown | found.
route_selection_status(P2PTransferState) ->
    case route(P2PTransferState) of
        undefined ->
            unknown;
        _Known ->
            found
    end.

-spec adjustments_index(p2p_transfer_state()) -> adjustments_index().
adjustments_index(P2PTransferState) ->
    case maps:find(adjustments, P2PTransferState) of
        {ok, Adjustments} ->
            Adjustments;
        error ->
            ff_adjustment_utils:new_index()
    end.

-spec set_adjustments_index(adjustments_index(), p2p_transfer_state()) -> p2p_transfer_state().
set_adjustments_index(Adjustments, P2PTransferState) ->
    P2PTransferState#{adjustments => Adjustments}.

-spec effective_final_cash_flow(p2p_transfer_state()) -> final_cash_flow().
effective_final_cash_flow(P2PTransferState) ->
    case ff_adjustment_utils:cash_flow(adjustments_index(P2PTransferState)) of
        undefined ->
            ff_cash_flow:make_empty_final();
        CashFlow ->
            CashFlow
    end.

%% Processing helpers

-spec deduce_activity(p2p_transfer_state()) -> activity().
deduce_activity(P2PTransferState) ->
    Params = #{
        risk_score => risk_score_status(P2PTransferState),
        route => route_selection_status(P2PTransferState),
        p_transfer => p_transfer_status(P2PTransferState),
        session => session_processing_status(P2PTransferState),
        status => status(P2PTransferState),
        active_adjustment => ff_adjustment_utils:is_active(adjustments_index(P2PTransferState))
    },
    do_deduce_activity(Params).

do_deduce_activity(#{status := pending} = Params) ->
    do_pending_activity(Params);
do_deduce_activity(#{status := succeeded} = Params) ->
    do_finished_activity(Params);
do_deduce_activity(#{status := {failed, _}} = Params) ->
    do_finished_activity(Params).

do_pending_activity(#{risk_score := unknown, p_transfer := undefined}) ->
    risk_scoring;
do_pending_activity(#{risk_score := scored, route := unknown, p_transfer := undefined}) ->
    routing;
do_pending_activity(#{route := found, p_transfer := undefined}) ->
    p_transfer_start;
do_pending_activity(#{p_transfer := created}) ->
    p_transfer_prepare;
do_pending_activity(#{p_transfer := prepared, session := undefined}) ->
    session_starting;
do_pending_activity(#{p_transfer := prepared, session := pending}) ->
    session_polling;
do_pending_activity(#{p_transfer := prepared, session := succeeded}) ->
    p_transfer_commit;
do_pending_activity(#{p_transfer := committed, session := succeeded}) ->
    finish;
do_pending_activity(#{p_transfer := prepared, session := failed}) ->
    p_transfer_cancel;
do_pending_activity(#{p_transfer := cancelled, session := failed}) ->
    {fail, session}.

do_finished_activity(#{active_adjustment := true}) ->
    adjustment.

-spec do_process_transfer(activity(), p2p_transfer_state()) -> process_result().
do_process_transfer(risk_scoring, P2PTransferState) ->
    process_risk_scoring(P2PTransferState);
do_process_transfer(routing, P2PTransferState) ->
    process_routing(P2PTransferState);
do_process_transfer(p_transfer_start, P2PTransferState) ->
    process_p_transfer_creation(P2PTransferState);
do_process_transfer(p_transfer_prepare, P2PTransferState) ->
    {ok, Events} = ff_pipeline:with(p_transfer, P2PTransferState, fun ff_postings_transfer:prepare/1),
    {continue, Events};
do_process_transfer(p_transfer_commit, P2PTransferState) ->
    {ok, Events} = ff_pipeline:with(p_transfer, P2PTransferState, fun ff_postings_transfer:commit/1),
    {continue, Events};
do_process_transfer(p_transfer_cancel, P2PTransferState) ->
    {ok, Events} = ff_pipeline:with(p_transfer, P2PTransferState, fun ff_postings_transfer:cancel/1),
    {continue, Events};
do_process_transfer(session_starting, P2PTransferState) ->
    process_session_creation(P2PTransferState);
do_process_transfer(session_polling, P2PTransferState) ->
    process_session_poll(P2PTransferState);
do_process_transfer({fail, Reason}, P2PTransferState) ->
    process_transfer_fail(Reason, P2PTransferState);
do_process_transfer(finish, P2PTransferState) ->
    process_transfer_finish(P2PTransferState);
do_process_transfer(adjustment, P2PTransferState) ->
    process_adjustment(P2PTransferState).

-spec process_risk_scoring(p2p_transfer_state()) -> process_result().
process_risk_scoring(P2PTransferState) ->
    RiskScore = do_risk_scoring(P2PTransferState),
    {continue, [
        {risk_score_changed, RiskScore}
    ]}.

-spec do_risk_scoring(p2p_transfer_state()) -> risk_score().
do_risk_scoring(P2PTransferState) ->
    DomainRevision = domain_revision(P2PTransferState),
    {ok, Identity} = get_identity(owner(P2PTransferState)),
    {ok, PaymentInstitutionID} = ff_party:get_identity_payment_institution_id(Identity),
    {ok, PaymentInstitution} = ff_payment_institution:get(PaymentInstitutionID, DomainRevision),
    PartyVarset = create_varset(Identity, P2PTransferState),
    {ok, InspectorRef} = ff_payment_institution:compute_p2p_inspector(PaymentInstitution, PartyVarset),
    {ok, Inspector} = ff_domain_config:object(
        DomainRevision,
        {p2p_inspector, #domain_P2PInspectorRef{id = InspectorRef}}
    ),
    Score =
        case genlib_app:env(p2p, score_id, undefined) of
            undefined ->
                _ = logger:warning("Fail to get env RiskScoreID set RiskScore to low"),
                high;
            ScoreID ->
                Scores = p2p_inspector:inspect(P2PTransferState, DomainRevision, [ScoreID], Inspector),
                maps:get(ScoreID, Scores)
        end,
    ff_dmsl_codec:unmarshal(risk_score, Score).

-spec process_routing(p2p_transfer_state()) -> process_result().
process_routing(P2PTransferState) ->
    case do_process_routing(P2PTransferState) of
        {ok, ProviderID} ->
            {continue, [
                {route_changed, #{
                    version => 1,
                    provider_id => ProviderID
                }}
            ]};
        {error, route_not_found} ->
            process_transfer_fail(route_not_found, P2PTransferState)
    end.

-spec do_process_routing(p2p_transfer_state()) -> {ok, provider_id()} | {error, route_not_found}.
do_process_routing(P2PTransferState) ->
    DomainRevision = domain_revision(P2PTransferState),
    {ok, Identity} = get_identity(owner(P2PTransferState)),

    do(fun() ->
        VarSet = create_varset(Identity, P2PTransferState),
        unwrap(prepare_route(VarSet, Identity, DomainRevision))
    end).

-spec prepare_route(party_varset(), identity(), domain_revision()) -> {ok, provider_id()} | {error, route_not_found}.
prepare_route(PartyVarset, Identity, DomainRevision) ->
    {ok, PaymentInstitutionID} = ff_party:get_identity_payment_institution_id(Identity),
    {ok, PaymentInstitution} = ff_payment_institution:get(PaymentInstitutionID, DomainRevision),
    {Routes, _RejectedContext} = ff_routing_rule:gather_routes(
        PaymentInstitution,
        p2p_transfer_routing_rules,
        PartyVarset,
        DomainRevision
    ),
    case Routes of
        [] ->
            {ok, Providers} = ff_payment_institution:compute_p2p_transfer_providers(PaymentInstitution, PartyVarset),
            choose_provider(Providers, PartyVarset);
        [_Route | _] ->
            Providers = ff_routing_rule:get_providers(Routes),
            choose_provider(Providers, PartyVarset)
    end.

-spec choose_provider([provider_id()], party_varset()) -> {ok, provider_id()} | {error, route_not_found}.
choose_provider(Providers, VS) ->
    case lists:filter(fun(P) -> validate_p2p_transfers_terms(P, VS) end, Providers) of
        [ProviderID | _] ->
            {ok, ProviderID};
        [] ->
            {error, route_not_found}
    end.

-spec validate_p2p_transfers_terms(provider_id(), party_varset()) -> boolean().
validate_p2p_transfers_terms(ID, VS) ->
    {ok, Provider} = ff_p2p_provider:get(ID),
    case ff_p2p_provider:validate_terms(Provider, VS) of
        {ok, valid} ->
            true;
        {error, _Error} ->
            false
    end.

-spec process_p_transfer_creation(p2p_transfer_state()) -> process_result().
process_p_transfer_creation(P2PTransferState) ->
    FinalCashFlow = make_final_cash_flow(P2PTransferState),
    PTransferID = construct_p_transfer_id(id(P2PTransferState)),
    {ok, PostingsTransferEvents} = ff_postings_transfer:create(PTransferID, FinalCashFlow),
    {continue, [{p_transfer, Ev} || Ev <- PostingsTransferEvents]}.

-spec process_session_creation(p2p_transfer_state()) -> process_result().
process_session_creation(P2PTransferState) ->
    ID = construct_session_id(id(P2PTransferState)),
    {ProviderFees, MerchantFees} = get_fees(P2PTransferState),
    TransferParams = genlib_map:compact(#{
        id => id(P2PTransferState),
        body => body(P2PTransferState),
        sender => sender_resource(P2PTransferState),
        receiver => receiver_resource(P2PTransferState),
        deadline => deadline(P2PTransferState),
        merchant_fees => MerchantFees,
        provider_fees => ProviderFees
    }),
    #{provider_id := ProviderID} = route(P2PTransferState),
    Params = #{
        route => #{
            provider_id => ProviderID
        },
        domain_revision => domain_revision(P2PTransferState),
        party_revision => party_revision(P2PTransferState)
    },
    case p2p_session_machine:create(ID, TransferParams, Params) of
        ok ->
            {continue, [{session, {ID, started}}]};
        {error, exists} ->
            {continue, [{session, {ID, started}}]}
    end.

construct_session_id(ID) ->
    ID.

-spec construct_p_transfer_id(id()) -> id().
construct_p_transfer_id(ID) ->
    <<"ff/p2p_transfer/", ID/binary>>.

-spec get_fees(p2p_transfer_state()) -> {ff_fees_final:fees() | undefined, ff_fees_final:fees() | undefined}.
get_fees(P2PTransferState) ->
    Route = route(P2PTransferState),
    #{provider_id := ProviderID} = Route,
    DomainRevision = domain_revision(P2PTransferState),
    {ok, Provider} = ff_p2p_provider:get(DomainRevision, ProviderID),
    {ok, Identity} = get_identity(owner(P2PTransferState)),
    PartyVarset = create_varset(Identity, P2PTransferState),
    Body = body(P2PTransferState),

    #{terms := ProviderTerms} = Provider,
    ProviderFees = get_provider_fees(ProviderTerms, Body, PartyVarset),

    PartyID = ff_identity:party(Identity),
    ContractID = ff_identity:contract(Identity),
    Timestamp = operation_timestamp(P2PTransferState),
    PartyRevision = party_revision(P2PTransferState),
    DomainRevision = domain_revision(P2PTransferState),
    {ok, Terms} = ff_party:get_contract_terms(
        PartyID,
        ContractID,
        PartyVarset,
        Timestamp,
        PartyRevision,
        DomainRevision
    ),
    #domain_TermSet{
        wallets = #domain_WalletServiceTerms{
            p2p = P2PMerchantTerms
        }
    } = Terms,
    MerchantFees = get_merchant_fees(P2PMerchantTerms, Body),
    {ProviderFees, MerchantFees}.

-spec get_provider_fees(dmsl_domain_thrift:'ProvisionTermSet'(), body(), p2p_party:varset()) ->
    ff_fees_final:fees() | undefined.
get_provider_fees(Terms, Body, PartyVarset) ->
    #domain_ProvisionTermSet{
        wallet = #domain_WalletProvisionTerms{
            p2p = P2PTerms
        }
    } = Terms,
    case P2PTerms of
        #domain_P2PProvisionTerms{fees = FeeSelector} ->
            {value, ProviderFees} = hg_selector:reduce(FeeSelector, PartyVarset),
            compute_fees(ProviderFees, Body);
        undefined ->
            undefined
    end.

-spec get_merchant_fees(dmsl_domain_thrift:'P2PServiceTerms'(), body()) -> ff_fees_final:fees() | undefined.
get_merchant_fees(#domain_P2PServiceTerms{fees = undefined}, _Body) ->
    undefined;
get_merchant_fees(#domain_P2PServiceTerms{fees = {value, MerchantFees}}, Body) ->
    compute_fees(MerchantFees, Body).

-spec compute_fees(dmsl_domain_thrift:'Fees'(), body()) -> ff_fees_final:fees().
compute_fees(Fees, Body) ->
    DecodedFees = ff_fees_plan:unmarshal(Fees),
    {ok, ComputedFees} = ff_fees_plan:compute(DecodedFees, Body),
    ComputedFees.

-spec process_session_poll(p2p_transfer_state()) -> process_result().
process_session_poll(P2PTransferState) ->
    SessionID = session_id(P2PTransferState),
    {ok, SessionMachine} = p2p_session_machine:get(SessionID),
    Session = p2p_session_machine:session(SessionMachine),
    case p2p_session:status(Session) of
        active ->
            {poll, []};
        {finished, Result} ->
            SessionID = session_id(P2PTransferState),
            {continue, [{session, {SessionID, {finished, Result}}}]}
    end.

-spec process_transfer_finish(p2p_transfer_state()) -> process_result().
process_transfer_finish(_P2PTransfer) ->
    {undefined, [{status_changed, succeeded}]}.

-spec process_transfer_fail(fail_type(), p2p_transfer_state()) -> process_result().
process_transfer_fail(FailType, P2PTransferState) ->
    Failure = build_failure(FailType, P2PTransferState),
    {undefined, [{status_changed, {failed, Failure}}]}.

-spec handle_child_result(process_result(), p2p_transfer_state()) -> process_result().
handle_child_result({undefined, Events} = Result, P2PTransferState) ->
    NextP2PTransfer = lists:foldl(fun(E, Acc) -> apply_event(E, Acc) end, P2PTransferState, Events),
    case is_active(NextP2PTransfer) of
        true ->
            {continue, Events};
        false ->
            Result
    end;
handle_child_result({_OtherAction, _Events} = Result, _P2PTransfer) ->
    Result.

-spec is_childs_active(p2p_transfer_state()) -> boolean().
is_childs_active(P2PTransferState) ->
    ff_adjustment_utils:is_active(adjustments_index(P2PTransferState)).

-spec make_final_cash_flow(p2p_transfer_state()) -> final_cash_flow().
make_final_cash_flow(P2PTransferState) ->
    Body = body(P2PTransferState),
    Route = route(P2PTransferState),
    DomainRevision = domain_revision(P2PTransferState),
    {ok, Identity} = get_identity(owner(P2PTransferState)),
    PartyID = ff_identity:party(Identity),
    PartyRevision = party_revision(P2PTransferState),
    ContractID = ff_identity:contract(Identity),
    Timestamp = operation_timestamp(P2PTransferState),
    PartyVarset = create_varset(Identity, P2PTransferState),

    {_Amount, CurrencyID} = Body,
    #{provider_id := ProviderID} = Route,
    {ok, Provider} = ff_p2p_provider:get(ProviderID),
    ProviderAccounts = ff_p2p_provider:accounts(Provider),
    ProviderAccount = maps:get(CurrencyID, ProviderAccounts, undefined),

    {ok, PaymentInstitutionID} = ff_party:get_identity_payment_institution_id(Identity),
    {ok, PaymentInstitution} = ff_payment_institution:get(PaymentInstitutionID, DomainRevision),
    {ok, SystemAccounts} = ff_payment_institution:compute_system_accounts(PaymentInstitution, PartyVarset),
    SystemAccount = maps:get(CurrencyID, SystemAccounts, #{}),
    SettlementAccount = maps:get(settlement, SystemAccount, undefined),
    SubagentAccount = maps:get(subagent, SystemAccount, undefined),

    ProviderFee = ff_p2p_provider:compute_fees(Provider, PartyVarset),

    {ok, Terms} = ff_party:get_contract_terms(
        PartyID,
        ContractID,
        PartyVarset,
        Timestamp,
        PartyRevision,
        DomainRevision
    ),
    {ok, P2PCashFlowPlan} = ff_party:get_p2p_cash_flow_plan(Terms),
    {ok, CashFlowPlan} = ff_cash_flow:add_fee(P2PCashFlowPlan, ProviderFee),
    Constants = #{
        operation_amount => Body
    },
    Accounts = genlib_map:compact(#{
        {system, settlement} => SettlementAccount,
        {system, subagent} => SubagentAccount,
        {provider, settlement} => ProviderAccount
    }),
    {ok, FinalCashFlow} = ff_cash_flow:finalize(CashFlowPlan, Accounts, Constants),
    FinalCashFlow.

-spec get_identity(identity_id()) -> {ok, identity()} | {error, notfound}.
get_identity(IdentityID) ->
    do(fun() ->
        IdentityMachine = unwrap(ff_identity_machine:get(IdentityID)),
        ff_identity_machine:identity(IdentityMachine)
    end).

-spec build_quote_state(quote() | undefined) -> quote_state() | undefined.
build_quote_state(undefined) ->
    undefined;
build_quote_state(Quote) ->
    #{
        fees => p2p_quote:fees(Quote),
        created_at => p2p_quote:created_at(Quote),
        expires_on => p2p_quote:expires_on(Quote),
        sender => p2p_quote:sender_descriptor(Quote),
        receiver => p2p_quote:receiver_descriptor(Quote)
    }.

%% Session management

-spec sessions(p2p_transfer_state()) -> [session()].
sessions(P2PTransferState) ->
    case session(P2PTransferState) of
        undefined ->
            [];
        Session ->
            [Session]
    end.

-spec session(p2p_transfer_state()) -> session() | undefined.
session(P2PTransferState) ->
    maps:get(session, P2PTransferState, undefined).

-spec session_id(p2p_transfer_state()) -> session_id() | undefined.
session_id(T) ->
    case session(T) of
        undefined ->
            undefined;
        #{id := SessionID} ->
            SessionID
    end.

-spec session_result(p2p_transfer_state()) -> session_result() | unknown | undefined.
session_result(P2PTransferState) ->
    case session(P2PTransferState) of
        undefined ->
            undefined;
        #{result := Result} ->
            Result;
        #{} ->
            unknown
    end.

-spec session_processing_status(p2p_transfer_state()) -> undefined | pending | succeeded | failed.
session_processing_status(P2PTransferState) ->
    case session_result(P2PTransferState) of
        undefined ->
            undefined;
        unknown ->
            pending;
        success ->
            succeeded;
        {failure, _Failure} ->
            failed
    end.

%% Adjustment validators

-spec validate_adjustment_start(adjustment_params(), p2p_transfer_state()) ->
    {ok, valid}
    | {error, start_adjustment_error()}.
validate_adjustment_start(Params, P2PTransferState) ->
    do(fun() ->
        valid = unwrap(validate_no_pending_adjustment(P2PTransferState)),
        valid = unwrap(validate_p2p_transfer_finish(P2PTransferState)),
        valid = unwrap(validate_status_change(Params, P2PTransferState))
    end).

-spec validate_p2p_transfer_finish(p2p_transfer_state()) ->
    {ok, valid}
    | {error, {invalid_p2p_transfer_status, status()}}.
validate_p2p_transfer_finish(P2PTransferState) ->
    case is_finished(P2PTransferState) of
        true ->
            {ok, valid};
        false ->
            {error, {invalid_p2p_transfer_status, status(P2PTransferState)}}
    end.

-spec validate_no_pending_adjustment(p2p_transfer_state()) ->
    {ok, valid}
    | {error, {another_adjustment_in_progress, adjustment_id()}}.
validate_no_pending_adjustment(P2PTransferState) ->
    case ff_adjustment_utils:get_not_finished(adjustments_index(P2PTransferState)) of
        error ->
            {ok, valid};
        {ok, AdjustmentID} ->
            {error, {another_adjustment_in_progress, AdjustmentID}}
    end.

-spec validate_status_change(adjustment_params(), p2p_transfer_state()) ->
    {ok, valid}
    | {error, invalid_status_change_error()}.
validate_status_change(#{change := {change_status, Status}}, P2PTransferState) ->
    do(fun() ->
        valid = unwrap(invalid_status_change, validate_target_status(Status)),
        valid = unwrap(invalid_status_change, validate_change_same_status(Status, status(P2PTransferState)))
    end);
validate_status_change(_Params, _P2PTransfer) ->
    {ok, valid}.

-spec validate_target_status(status()) ->
    {ok, valid}
    | {error, {unavailable_status, status()}}.
validate_target_status(succeeded) ->
    {ok, valid};
validate_target_status({failed, _Failure}) ->
    {ok, valid};
validate_target_status(Status) ->
    {error, {unavailable_status, Status}}.

-spec validate_change_same_status(status(), status()) ->
    {ok, valid}
    | {error, {already_has_status, status()}}.
validate_change_same_status(NewStatus, OldStatus) when NewStatus =/= OldStatus ->
    {ok, valid};
validate_change_same_status(Status, Status) ->
    {error, {already_has_status, Status}}.

%% Adjustment helpers

-spec apply_adjustment_event(wrapped_adjustment_event(), p2p_transfer_state()) -> p2p_transfer_state().
apply_adjustment_event(WrappedEvent, P2PTransferState) ->
    Adjustments0 = adjustments_index(P2PTransferState),
    Adjustments1 = ff_adjustment_utils:apply_event(WrappedEvent, Adjustments0),
    set_adjustments_index(Adjustments1, P2PTransferState).

-spec make_adjustment_params(adjustment_params(), p2p_transfer_state()) -> ff_adjustment:params().
make_adjustment_params(Params, P2PTransferState) ->
    #{id := ID, change := Change} = Params,
    genlib_map:compact(#{
        id => ID,
        changes_plan => make_adjustment_change(Change, P2PTransferState),
        external_id => genlib_map:get(external_id, Params),
        domain_revision => domain_revision(P2PTransferState),
        party_revision => party_revision(P2PTransferState),
        operation_timestamp => created_at(P2PTransferState)
    }).

-spec make_adjustment_change(adjustment_change(), p2p_transfer_state()) -> ff_adjustment:changes().
make_adjustment_change({change_status, NewStatus}, P2PTransferState) ->
    CurrentStatus = status(P2PTransferState),
    make_change_status_params(CurrentStatus, NewStatus, P2PTransferState).

-spec make_change_status_params(status(), status(), p2p_transfer_state()) -> ff_adjustment:changes().
make_change_status_params(succeeded, {failed, _} = NewStatus, P2PTransferState) ->
    CurrentCashFlow = effective_final_cash_flow(P2PTransferState),
    NewCashFlow = ff_cash_flow:make_empty_final(),
    #{
        new_status => #{
            new_status => NewStatus
        },
        new_cash_flow => #{
            old_cash_flow_inverted => ff_cash_flow:inverse(CurrentCashFlow),
            new_cash_flow => NewCashFlow
        }
    };
make_change_status_params({failed, _}, succeeded = NewStatus, P2PTransferState) ->
    CurrentCashFlow = effective_final_cash_flow(P2PTransferState),
    NewCashFlow = make_final_cash_flow(P2PTransferState),
    #{
        new_status => #{
            new_status => NewStatus
        },
        new_cash_flow => #{
            old_cash_flow_inverted => ff_cash_flow:inverse(CurrentCashFlow),
            new_cash_flow => NewCashFlow
        }
    };
make_change_status_params({failed, _}, {failed, _} = NewStatus, _P2PTransfer) ->
    #{
        new_status => #{
            new_status => NewStatus
        }
    }.

-spec process_adjustment(p2p_transfer_state()) -> process_result().
process_adjustment(P2PTransferState) ->
    #{
        action := Action,
        events := Events0,
        changes := Changes
    } = ff_adjustment_utils:process_adjustments(adjustments_index(P2PTransferState)),
    Events1 = Events0 ++ handle_adjustment_changes(Changes),
    handle_child_result({Action, Events1}, P2PTransferState).

-spec handle_adjustment_changes(ff_adjustment:changes()) -> [event()].
handle_adjustment_changes(Changes) ->
    StatusChange = maps:get(new_status, Changes, undefined),
    handle_adjustment_status_change(StatusChange).

-spec handle_adjustment_status_change(ff_adjustment:status_change() | undefined) -> [event()].
handle_adjustment_status_change(undefined) ->
    [];
handle_adjustment_status_change(#{new_status := Status}) ->
    [{status_changed, Status}].

-spec save_adjustable_info(event(), p2p_transfer_state()) -> p2p_transfer_state().
save_adjustable_info({p_transfer, {status_changed, committed}}, P2PTransferState) ->
    CashFlow = ff_postings_transfer:final_cash_flow(p_transfer(P2PTransferState)),
    update_adjusment_index(fun ff_adjustment_utils:set_cash_flow/2, CashFlow, P2PTransferState);
save_adjustable_info(_Ev, P2PTransferState) ->
    P2PTransferState.

-spec update_adjusment_index(Updater, Value, p2p_transfer_state()) -> p2p_transfer_state() when
    Updater :: fun((Value, adjustments_index()) -> adjustments_index()),
    Value :: any().
update_adjusment_index(Updater, Value, P2PTransferState) ->
    Index = adjustments_index(P2PTransferState),
    set_adjustments_index(Updater(Value, Index), P2PTransferState).

%% Failure helpers

-spec build_failure(fail_type(), p2p_transfer_state()) -> failure().
build_failure(route_not_found, _P2PTransfer) ->
    #{
        code => <<"no_route_found">>
    };
build_failure(session, P2PTransferState) ->
    Result = session_result(P2PTransferState),
    {failure, Failure} = Result,
    Failure.

validate_definition(Tag, undefined) ->
    error({Tag, undefined});
validate_definition(_Tag, Value) ->
    Value.

%%

-spec apply_event(event() | legacy_event(), ff_maybe:maybe(p2p_transfer_state())) -> p2p_transfer_state().
apply_event(Ev, T0) ->
    T1 = apply_event_(Ev, T0),
    T2 = save_adjustable_info(Ev, T1),
    T2.

-spec apply_event_(event(), ff_maybe:maybe(p2p_transfer_state())) -> p2p_transfer_state().
apply_event_({created, T}, undefined) ->
    T;
apply_event_({status_changed, Status}, T) ->
    maps:put(status, Status, T);
apply_event_({resource_got, Sender, Receiver}, T0) ->
    T1 = maps:put(sender_resource, Sender, T0),
    maps:put(receiver_resource, Receiver, T1);
apply_event_({p_transfer, Ev}, T) ->
    T#{p_transfer => ff_postings_transfer:apply_event(Ev, p_transfer(T))};
apply_event_({session, {SessionID, started}}, T) ->
    Session = #{id => SessionID},
    maps:put(session, Session, T);
apply_event_({session, {SessionID, {finished, Result}}}, T) ->
    #{id := SessionID} = Session = session(T),
    maps:put(session, Session#{result => Result}, T);
apply_event_({risk_score_changed, RiskScore}, T) ->
    maps:put(risk_score, RiskScore, T);
apply_event_({route_changed, Route}, T) ->
    maps:put(route, Route, T);
apply_event_({adjustment, _Ev} = Event, T) ->
    apply_adjustment_event(Event, T).
