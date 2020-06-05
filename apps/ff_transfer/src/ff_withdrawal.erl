%%%
%%% Withdrawal
%%%

-module(ff_withdrawal).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_withdrawals_provider_adapter_thrift.hrl").

-type id() :: binary().
-type clock() :: ff_transaction:clock().

-define(ACTUAL_FORMAT_VERSION, 3).
-opaque withdrawal_state() :: #{
    id              := id(),
    transfer_type   := withdrawal,
    body            := body(),
    params          := transfer_params(),
    created_at      => ff_time:timestamp_ms(),
    party_revision  => party_revision(),
    domain_revision => domain_revision(),
    route           => route(),
    routes          => routes(),
    resource        => destination_resource(),
    adjustments     => adjustments_index(),
    status          => status(),
    metadata        => metadata(),
    external_id     => id()
}.

-opaque withdrawal() :: #{
    version         := ?ACTUAL_FORMAT_VERSION,
    id              := id(),
    transfer_type   := withdrawal,
    body            := body(),
    params          := transfer_params(),
    created_at      => ff_time:timestamp_ms(),
    party_revision  => party_revision(),
    domain_revision => domain_revision(),
    route           => route(),
    metadata        => metadata(),
    external_id     => id()
}.

-type params() :: #{
    id                   := id(),
    wallet_id            := ff_wallet_machine:id(),
    destination_id       := ff_destination:id(),
    body                 := body(),
    external_id          => id(),
    quote                => quote(),
    metadata             => metadata()
}.

-type status() ::
    pending         |
    succeeded       |
    {failed, failure()} .

-type event() ::
    {created, withdrawal()} |
    {resource_got, destination_resource()} |
    {route_changed, route()} |
    {p_transfer, ff_postings_transfer:event()} |
    {limit_check, limit_check_details()} |
    {session_started, session_id()} |
    {session_finished, {session_id(), session_result()}} |
    {status_changed, status()} |
    wrapped_adjustment_event().


-type create_error() ::
    {wallet, notfound} |
    {destination, notfound | unauthorized} |
    {inconsistent_currency, {Withdrawal :: currency_id(), Wallet :: currency_id(), Destination :: currency_id()}} |
    {terms, ff_party:validate_withdrawal_creation_error()} |
    {destination_resource, {bin_data, not_found}}.

-type route() :: #{
    provider_id := provider_id(),
    pending_providers => [provider_id()]
}.

-type routes() :: ff_withdrawal_route_utils:routes().

-type prepared_route() :: #{
    route := route(),
    party_revision := party_revision(),
    domain_revision := domain_revision()
}.

-type quote_params() :: #{
    wallet_id      := ff_wallet_machine:id(),
    currency_from  := ff_currency:id(),
    currency_to    := ff_currency:id(),
    body           := ff_transaction:body(),
    destination_id => ff_destination:id(),
    external_id    => id()
}.

-type quote() :: ff_adapter_withdrawal:quote(quote_validation_data()).

-type session() :: #{
    id     := session_id(),
    result => session_result()
}.

-type gen_args() :: #{
    id              := id(),
    body            := body(),
    params          := params(),
    transfer_type   := withdrawal,

    status          => status(),
    route           => route(),
    external_id     => external_id(),
    created_at      => ff_time:timestamp_ms(),
    party_revision  => party_revision(),
    domain_revision => domain_revision(),
    metadata        => metadata()
}.

-type limit_check_details() ::
    {wallet_sender, wallet_limit_check_details()}.

-type wallet_limit_check_details() ::
    ok |
    {failed, wallet_limit_check_error()}.

-type wallet_limit_check_error() :: #{
    expected_range := cash_range(),
    balance := cash()
}.

-type adjustment_params() :: #{
    id          := adjustment_id(),
    change      := adjustment_change(),
    external_id => id()
}.

-type adjustment_change() ::
    {change_status, status()}.

-type start_adjustment_error() ::
    invalid_withdrawal_status_error() |
    invalid_status_change_error() |
    {another_adjustment_in_progress, adjustment_id()} |
    ff_adjustment:create_error().

-type unknown_adjustment_error() :: ff_adjustment_utils:unknown_adjustment_error().

-type invalid_status_change_error() ::
    {invalid_status_change, {unavailable_status, status()}} |
    {invalid_status_change, {already_has_status, status()}}.

-type invalid_withdrawal_status_error() ::
    {invalid_withdrawal_status, status()}.

-type action() :: poll | continue | undefined.

-export_type([withdrawal/0]).
-export_type([withdrawal_state/0]).
-export_type([id/0]).
-export_type([params/0]).
-export_type([event/0]).
-export_type([route/0]).
-export_type([prepared_route/0]).
-export_type([quote/0]).
-export_type([quote_params/0]).
-export_type([session/0]).
-export_type([gen_args/0]).
-export_type([create_error/0]).
-export_type([action/0]).
-export_type([adjustment_params/0]).
-export_type([start_adjustment_error/0]).
-export_type([limit_check_details/0]).

%% Transfer logic callbacks

-export([process_transfer/1]).

%% Accessors

-export([wallet_id/1]).
-export([destination_id/1]).
-export([quote/1]).
-export([id/1]).
-export([body/1]).
-export([status/1]).
-export([route/1]).
-export([routes/1]).
-export([update_routes/2]).
-export([external_id/1]).
-export([created_at/1]).
-export([party_revision/1]).
-export([domain_revision/1]).
-export([destination_resource/1]).
-export([metadata/1]).

%% API

-export([create/1]).
-export([gen/1]).
-export([get_quote/1]).
-export([is_finished/1]).

-export([start_adjustment/2]).
-export([find_adjustment/2]).
-export([adjustments/1]).
-export([effective_final_cash_flow/1]).
-export([sessions/1]).

%% Event source

-export([apply_event/2]).
-export([maybe_migrate/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2, valid/2]).

%% Internal types

-type body()                  :: ff_transaction:body().
-type identity()              :: ff_identity:identity_state().
-type party_id()              :: ff_party:id().
-type wallet_id()             :: ff_wallet:id().
-type wallet()                :: ff_wallet:wallet_state().
-type destination_id()        :: ff_destination:id().
-type destination()           :: ff_destination:destination_state().
-type process_result()        :: {action(), [event()]}.
-type final_cash_flow()       :: ff_cash_flow:final_cash_flow().
-type external_id()           :: id() | undefined.
-type p_transfer()            :: ff_postings_transfer:transfer().
-type session_id()            :: id().
-type destination_resource()  :: ff_destination:resource_full().
-type cash()                  :: ff_cash:cash().
-type cash_range()            :: ff_range:range(cash()).
-type failure()               :: ff_failure:failure().
-type session_result()        :: ff_withdrawal_session:session_result().
-type adjustment()            :: ff_adjustment:adjustment().
-type adjustment_id()         :: ff_adjustment:id().
-type adjustments_index()     :: ff_adjustment_utils:index().
-type currency_id()           :: ff_currency:id().
-type party_revision()        :: ff_party:revision().
-type domain_revision()       :: ff_domain_config:revision().
-type terms()                 :: ff_party:terms().
-type party_varset()          :: hg_selector:varset().
-type metadata()              :: ff_entity_context:md().

-type wrapped_adjustment_event()  :: ff_adjustment_utils:wrapped_event().

% TODO I'm now sure about this change, it may crash old events. Or not. ))
-type provider_id() :: pos_integer() | id().

-type legacy_event() :: any().

-type transfer_params() :: #{
    wallet_id      := wallet_id(),
    destination_id := destination_id(),
    quote          => quote()
}.

-type quote_validation_data() :: #{
    binary() => any()
}.

-type party_varset_params() :: #{
    body := body(),
    wallet_id := wallet_id(),
    party_id := party_id(),
    destination => destination(),
    resource => destination_resource()
}.

-type activity() ::
    routing |
    p_transfer_start |
    p_transfer_prepare |
    session_starting |
    session_polling |
    p_transfer_commit |
    p_transfer_cancel |
    limit_check |
    {fail, fail_type()} |
    adjustment |
    stop |  % Legacy activity
    finish.

-type fail_type() ::
    limit_check |
    route_not_found |
    {inconsistent_quote_route, provider_id()} |
    session.

%% Accessors

-spec wallet_id(withdrawal_state()) -> wallet_id().
wallet_id(T) ->
    maps:get(wallet_id, params(T)).

-spec destination_id(withdrawal_state()) -> destination_id().
destination_id(T) ->
    maps:get(destination_id, params(T)).

-spec destination_resource(withdrawal_state()) ->
    destination_resource().
destination_resource(#{resource := Resource}) ->
    Resource;
destination_resource(Withdrawal) ->
    DestinationID = destination_id(Withdrawal),
    {ok, DestinationMachine} = ff_destination:get_machine(DestinationID),
    Destination = ff_destination:get(DestinationMachine),
    {ok, Resource} = ff_destination:resource_full(Destination),
    Resource.

%%

-spec quote(withdrawal_state()) -> quote() | undefined.
quote(T) ->
    maps:get(quote, params(T), undefined).

-spec id(withdrawal_state()) -> id().
id(#{id := V}) ->
    V.

-spec body(withdrawal_state()) -> body().
body(#{body := V}) ->
    V.

-spec status(withdrawal_state()) -> status() | undefined.
status(T) ->
    maps:get(status, T, undefined).

-spec route(withdrawal_state()) -> route() | undefined.
route(T) ->
    maps:get(route, T, undefined).

-spec routes(withdrawal_state()) -> routes() | undefined.
routes(T) ->
    maps:get(routes, T, undefined).

-spec update_routes(routes(), withdrawal_state()) -> withdrawal_state().
update_routes(Routes, T) ->
    maps:put(routes, Routes, T).

-spec external_id(withdrawal_state()) -> external_id() | undefined.
external_id(T) ->
    maps:get(external_id, T, undefined).

-spec party_revision(withdrawal_state()) -> party_revision() | undefined.
party_revision(T) ->
    maps:get(party_revision, T, undefined).

-spec domain_revision(withdrawal_state()) -> domain_revision() | undefined.
domain_revision(T) ->
    maps:get(domain_revision, T, undefined).

-spec created_at(withdrawal_state()) -> ff_time:timestamp_ms() | undefined.
created_at(T) ->
    maps:get(created_at, T, undefined).

-spec metadata(withdrawal_state()) -> metadata() | undefined.
metadata(T) ->
    maps:get(metadata, T, undefined).

%% API

-spec gen(gen_args()) ->
    withdrawal().
gen(Args) ->
    TypeKeys = [
        id, transfer_type, body, params, external_id,
        domain_revision, party_revision, created_at, route, metadata
    ],
    genlib_map:compact(maps:with(TypeKeys, Args)).

-spec create(params()) ->
    {ok, [event()]} |
    {error, create_error()}.
create(Params) ->
    do(fun() ->
        #{id := ID, wallet_id := WalletID, destination_id := DestinationID, body := Body} = Params,
        CreatedAt = ff_time:now(),
        Quote = maps:get(quote, Params, undefined),
        ResourceID = quote_resource_id(Quote),
        Timestamp = ff_maybe:get_defined(quote_timestamp(Quote), CreatedAt),
        DomainRevision = ensure_domain_revision_defined(quote_domain_revision(Quote)),
        Wallet = unwrap(wallet, get_wallet(WalletID)),
        Destination = unwrap(destination, get_destination(DestinationID)),
        Resource = unwrap(destination_resource, ff_destination:resource_full(Destination, ResourceID)),

        Identity = get_wallet_identity(Wallet),
        PartyID = ff_identity:party(get_wallet_identity(Wallet)),
        PartyRevision = ensure_party_revision_defined(PartyID, quote_party_revision(Quote)),
        ContractID = ff_identity:contract(Identity),
        VarsetParams = genlib_map:compact(#{
            body => Body,
            wallet_id => WalletID,
            party_id => PartyID,
            destination => Destination,
            resource => Resource
        }),
        {ok, Terms} = ff_party:get_contract_terms(
            PartyID, ContractID, build_party_varset(VarsetParams), Timestamp, PartyRevision, DomainRevision
        ),
        valid = unwrap(validate_withdrawal_creation(Terms, Body, Wallet, Destination)),

        TransferParams = genlib_map:compact(#{
            wallet_id => WalletID,
            destination_id => DestinationID,
            quote => Quote
        }),
        [
            {created, genlib_map:compact(#{
                version         => ?ACTUAL_FORMAT_VERSION,
                id              => ID,
                transfer_type   => withdrawal,
                body            => Body,
                params          => TransferParams,
                created_at      => CreatedAt,
                party_revision  => PartyRevision,
                domain_revision => DomainRevision,
                external_id     => maps:get(external_id, Params, undefined),
                metadata        => maps:get(metadata, Params, undefined)
            })},
            {status_changed, pending},
            {resource_got, Resource}
        ]
    end).

-spec start_adjustment(adjustment_params(), withdrawal_state()) ->
    {ok, process_result()} |
    {error, start_adjustment_error()}.
start_adjustment(Params, Withdrawal) ->
    #{id := AdjustmentID} = Params,
    case find_adjustment(AdjustmentID, Withdrawal) of
        {error, {unknown_adjustment, _}} ->
            do_start_adjustment(Params, Withdrawal);
        {ok, _Adjustment} ->
            {ok, {undefined, []}}
    end.

-spec find_adjustment(adjustment_id(), withdrawal_state()) ->
    {ok, adjustment()} | {error, unknown_adjustment_error()}.
find_adjustment(AdjustmentID, Withdrawal) ->
    ff_adjustment_utils:get_by_id(AdjustmentID, adjustments_index(Withdrawal)).

-spec adjustments(withdrawal_state()) -> [adjustment()].
adjustments(Withdrawal) ->
    ff_adjustment_utils:adjustments(adjustments_index(Withdrawal)).

-spec effective_final_cash_flow(withdrawal_state()) -> final_cash_flow().
effective_final_cash_flow(Withdrawal) ->
    case ff_adjustment_utils:cash_flow(adjustments_index(Withdrawal)) of
        undefined ->
            ff_cash_flow:make_empty_final();
        CashFlow ->
            CashFlow
    end.

-spec sessions(withdrawal_state()) -> [session()].
sessions(Withdrawal) ->
    ff_withdrawal_route_utils:get_sessions(Withdrawal).

%% Сущность в настоящий момент нуждается в передаче ей управления для совершения каких-то действий
-spec is_active(withdrawal_state()) -> boolean().
is_active(#{status := succeeded} = Withdrawal) ->
    is_childs_active(Withdrawal);
is_active(#{status := {failed, _}} = Withdrawal) ->
    is_childs_active(Withdrawal);
is_active(#{status := pending}) ->
    true.

%% Сущность завершила свою основную задачу по переводу денег. Дальше её состояние будет меняться только
%% изменением дочерних сущностей, например запуском adjustment.
-spec is_finished(withdrawal_state()) -> boolean().
is_finished(#{status := succeeded}) ->
    true;
is_finished(#{status := {failed, _}}) ->
    true;
is_finished(#{status := pending}) ->
    false.

%% Transfer callbacks

-spec process_transfer(withdrawal_state()) ->
    process_result().
process_transfer(Withdrawal) ->
    Activity = deduce_activity(Withdrawal),
    do_process_transfer(Activity, Withdrawal).

%% Internals

-spec do_start_adjustment(adjustment_params(), withdrawal_state()) ->
    {ok, process_result()} |
    {error, start_adjustment_error()}.
do_start_adjustment(Params, Withdrawal) ->
    do(fun() ->
        valid = unwrap(validate_adjustment_start(Params, Withdrawal)),
        AdjustmentParams = make_adjustment_params(Params, Withdrawal),
        #{id := AdjustmentID} = Params,
        {Action, Events} = unwrap(ff_adjustment:create(AdjustmentParams)),
        {Action, ff_adjustment_utils:wrap_events(AdjustmentID, Events)}
    end).

%% Internal getters

-spec params(withdrawal_state()) -> transfer_params().
params(#{params := V}) ->
    V.

-spec p_transfer(withdrawal_state()) -> p_transfer() | undefined.
p_transfer(Withdrawal) ->
    ff_withdrawal_route_utils:get_current_p_transfer(Withdrawal).

-spec p_transfer_status(withdrawal_state()) -> ff_postings_transfer:status() | undefined.
p_transfer_status(Withdrawal) ->
    case routes(Withdrawal) of
        undefined ->
            undefined;
        _ ->
            p_transfer_status_(Withdrawal)
    end.

p_transfer_status_(Withdrawal) ->
    case p_transfer(Withdrawal) of
        undefined ->
            undefined;
        Transfer ->
            ff_postings_transfer:status(Transfer)
    end.

-spec route_selection_status(withdrawal_state()) -> unknown | found.
route_selection_status(Withdrawal) ->
    case route(Withdrawal) of
        undefined ->
            unknown;
        _Known ->
            found
    end.

-spec has_pending_routes(withdrawal_state()) -> boolean().

has_pending_routes(Withdrawal) ->
    Route = route(Withdrawal),
    [] =/= maps:get(pending_providers, Route, []).

-spec adjustments_index(withdrawal_state()) -> adjustments_index().
adjustments_index(Withdrawal) ->
    case maps:find(adjustments, Withdrawal) of
        {ok, Adjustments} ->
            Adjustments;
        error ->
            ff_adjustment_utils:new_index()
    end.

-spec set_adjustments_index(adjustments_index(), withdrawal_state()) -> withdrawal_state().
set_adjustments_index(Adjustments, Withdrawal) ->
    Withdrawal#{adjustments => Adjustments}.

-spec operation_timestamp(withdrawal_state()) -> ff_time:timestamp_ms().
operation_timestamp(Withdrawal) ->
    QuoteTimestamp = quote_timestamp(quote(Withdrawal)),
    ff_maybe:get_defined([QuoteTimestamp, created_at(Withdrawal), ff_time:now()]).

-spec operation_party_revision(withdrawal_state()) ->
    domain_revision().
operation_party_revision(Withdrawal) ->
    case party_revision(Withdrawal) of
        undefined ->
            {ok, Wallet} = get_wallet(wallet_id(Withdrawal)),
            PartyID = ff_identity:party(get_wallet_identity(Wallet)),
            {ok, Revision} = ff_party:get_revision(PartyID),
            Revision;
        Revision ->
            Revision
    end.

-spec operation_domain_revision(withdrawal_state()) ->
    domain_revision().
operation_domain_revision(Withdrawal) ->
    case domain_revision(Withdrawal) of
        undefined ->
            ff_domain_config:head();
        Revision ->
            Revision
    end.

%% Processing helpers

-spec deduce_activity(withdrawal_state()) ->
    activity().
deduce_activity(Withdrawal) ->
    Params = #{
        route => route_selection_status(Withdrawal),
        p_transfer => p_transfer_status(Withdrawal),
        session => session_processing_status(Withdrawal),
        status => status(Withdrawal),
        limit_check => limit_check_processing_status(Withdrawal),
        active_adjustment => ff_adjustment_utils:is_active(adjustments_index(Withdrawal))
    },
    do_deduce_activity(Params).

do_deduce_activity(#{status := pending} = Params) ->
    do_pending_activity(Params);
do_deduce_activity(#{status := succeeded} = Params) ->
    do_finished_activity(Params);
do_deduce_activity(#{status := {failed, _}} = Params) ->
    do_finished_activity(Params).

do_pending_activity(#{route := unknown, p_transfer := undefined}) ->
    routing;
do_pending_activity(#{route := found, p_transfer := undefined}) ->
    p_transfer_start;
do_pending_activity(#{p_transfer := created}) ->
    p_transfer_prepare;
do_pending_activity(#{p_transfer := prepared, limit_check := unknown}) ->
    limit_check;
do_pending_activity(#{p_transfer := prepared, limit_check:= ok, session := undefined}) ->
    session_starting;
do_pending_activity(#{p_transfer := prepared, limit_check := failed}) ->
    p_transfer_cancel;
do_pending_activity(#{p_transfer := cancelled, limit_check := failed}) ->
    {fail, limit_check};
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
    adjustment;
%% Legacy activity. Remove after first deployment
do_finished_activity(#{status := {failed, _}, p_transfer := prepared}) ->
    p_transfer_cancel;
do_finished_activity(#{status := succeeded, p_transfer := prepared}) ->
    p_transfer_commit;
do_finished_activity(#{status := succeeded, p_transfer := committed}) ->
    stop;
do_finished_activity(#{status := {failed, _}, p_transfer := cancelled}) ->
    stop.

-spec do_process_transfer(activity(), withdrawal_state()) ->
    process_result().
do_process_transfer(routing, Withdrawal) ->
    process_routing(Withdrawal);
do_process_transfer(p_transfer_start, Withdrawal) ->
    process_p_transfer_creation(Withdrawal);
do_process_transfer(p_transfer_prepare, Withdrawal) ->
    Tr = ff_withdrawal_route_utils:get_current_p_transfer(Withdrawal),
    {ok, Events} = ff_postings_transfer:prepare(Tr),
    {continue, [{p_transfer, Ev} || Ev <- Events]};
do_process_transfer(p_transfer_commit, Withdrawal) ->
    Tr = ff_withdrawal_route_utils:get_current_p_transfer(Withdrawal),
    {ok, Events} = ff_postings_transfer:commit(Tr),
    {continue, [{p_transfer, Ev} || Ev <- Events]};
do_process_transfer(p_transfer_cancel, Withdrawal) ->
    Tr = ff_withdrawal_route_utils:get_current_p_transfer(Withdrawal),
    {ok, Events} = ff_postings_transfer:cancel(Tr),
    {continue, [{p_transfer, Ev} || Ev <- Events]};
do_process_transfer(limit_check, Withdrawal) ->
    process_limit_check(Withdrawal);
do_process_transfer(session_starting, Withdrawal) ->
    process_session_creation(Withdrawal);
do_process_transfer(session_polling, Withdrawal) ->
    process_session_poll(Withdrawal);
do_process_transfer({fail, Reason}, Withdrawal) ->
    case has_pending_routes(Withdrawal) of
        false ->
            process_transfer_fail(Reason, Withdrawal);
        true ->
            {_, [FailEvent]} = process_transfer_fail(Reason, Withdrawal),
            {continue, [FailEvent | process_route_change(Withdrawal)]}
    end;
do_process_transfer(finish, Withdrawal) ->
    process_transfer_finish(Withdrawal);
do_process_transfer(adjustment, Withdrawal) ->
    process_adjustment(Withdrawal);
do_process_transfer(stop, _Withdrawal) ->
    {undefined, []}.

-spec process_routing(withdrawal_state()) ->
    process_result().
process_routing(Withdrawal) ->
    case do_process_routing(Withdrawal) of
        {ok, [ProviderID | Providers]} ->
            {continue, [
                {route_changed, #{provider_id => ProviderID, pending_providers => Providers}}
            ]};
        {error, route_not_found} ->
            process_transfer_fail(route_not_found, Withdrawal);
        {error, {inconsistent_quote_route, _ProviderID} = Reason} ->
            process_transfer_fail(Reason, Withdrawal)
    end.

-spec do_process_routing(withdrawal_state()) -> {ok, [provider_id()]} | {error, Reason} when
    Reason :: route_not_found | {inconsistent_quote_route, provider_id()}.
do_process_routing(Withdrawal) ->
    WalletID = wallet_id(Withdrawal),
    {ok, Wallet} = get_wallet(WalletID),
    DomainRevision = operation_domain_revision(Withdrawal),
    {ok, Destination} = get_destination(destination_id(Withdrawal)),
    Resource = destination_resource(Withdrawal),
    Identity = get_wallet_identity(Wallet),
    PartyID = ff_identity:party(get_wallet_identity(Wallet)),
    VarsetParams = genlib_map:compact(#{
        body => body(Withdrawal),
        wallet_id => WalletID,
        wallet => Wallet,
        party_id => PartyID,
        destination => Destination,
        resource => Resource
    }),

    do(fun() ->
        Providers = unwrap(prepare_route(build_party_varset(VarsetParams), Identity, DomainRevision)),
        case quote(Withdrawal) of
            undefined ->
                Providers;
            Quote ->
                ProviderID = hd(Providers),
                valid = unwrap(validate_quote_provider(ProviderID, Quote)),
                [ProviderID]
        end
    end).

-spec prepare_route(party_varset(), identity(), domain_revision()) ->
    {ok, [provider_id()]} | {error, route_not_found}.

prepare_route(PartyVarset, Identity, DomainRevision) ->
    {ok, PaymentInstitutionID} = ff_party:get_identity_payment_institution_id(Identity),
    {ok, PaymentInstitution} = ff_payment_institution:get(PaymentInstitutionID, DomainRevision),
    case ff_payment_institution:compute_withdrawal_providers(PaymentInstitution, PartyVarset) of
        {ok, Providers}  ->
            choose_provider(Providers, PartyVarset);
        {error, {misconfiguration, _Details} = Error} ->
            %% TODO: Do not interpret such error as an empty route list.
            %% The current implementation is made for compatibility reasons.
            %% Try to remove and follow the tests.
            _ = logger:warning("Route search failed: ~p", [Error]),
            {error, route_not_found}
    end.

-spec validate_quote_provider(provider_id(), quote()) ->
    {ok, valid} | {error, {inconsistent_quote_route, provider_id()}}.
validate_quote_provider(ProviderID, #{quote_data := #{<<"provider_id">> := ProviderID}}) ->
    {ok, valid};
validate_quote_provider(ProviderID, _) ->
    {error, {inconsistent_quote_route, ProviderID}}.

-spec choose_provider([provider_id()], party_varset()) ->
    {ok, [provider_id()]} | {error, route_not_found}.
choose_provider(Providers, VS) ->
    case lists:filter(fun(P) -> validate_withdrawals_terms(P, VS) end, Providers) of
        [] ->
            {error, route_not_found};
        Providers ->
            {ok, Providers}
    end.

-spec validate_withdrawals_terms(provider_id(), party_varset()) ->
    boolean().
validate_withdrawals_terms(ID, VS) ->
    Provider = unwrap(ff_payouts_provider:get(ID)),
    case ff_payouts_provider:validate_terms(Provider, VS) of
        {ok, valid} ->
            true;
        {error, _Error} ->
            false
    end.

-spec process_limit_check(withdrawal_state()) ->
    process_result().
process_limit_check(Withdrawal) ->
    WalletID = wallet_id(Withdrawal),
    {ok, Wallet} = get_wallet(WalletID),
    DomainRevision = operation_domain_revision(Withdrawal),
    {ok, Destination} = get_destination(destination_id(Withdrawal)),
    Resource = destination_resource(Withdrawal),
    Identity = get_wallet_identity(Wallet),
    PartyID = ff_identity:party(get_wallet_identity(Wallet)),
    PartyRevision = operation_party_revision(Withdrawal),
    ContractID = ff_identity:contract(Identity),
    Timestamp = operation_timestamp(Withdrawal),
    VarsetParams = genlib_map:compact(#{
        body => body(Withdrawal),
        wallet_id => WalletID,
        wallet => Wallet,
        party_id => PartyID,
        destination => Destination,
        resource => Resource
    }),
    PartyVarset = build_party_varset(VarsetParams),
    {ok, Terms} = ff_party:get_contract_terms(
        PartyID, ContractID, PartyVarset, Timestamp, PartyRevision, DomainRevision
    ),
    Clock = ff_postings_transfer:clock(p_transfer(Withdrawal)),
    Events = case validate_wallet_limits(Terms, Wallet, Clock) of
        {ok, valid} ->
            [{limit_check, {wallet_sender, ok}}];
        {error, {terms_violation, {wallet_limit, {cash_range, {Cash, Range}}}}} ->
            Details = #{
                expected_range => Range,
                balance => Cash
            },
            [{limit_check, {wallet_sender, {failed, Details}}}]
    end,
    {continue, Events}.

-spec process_p_transfer_creation(withdrawal_state()) ->
    process_result().
process_p_transfer_creation(Withdrawal) ->
    FinalCashFlow = make_final_cash_flow(Withdrawal),
    PTransferID = construct_p_transfer_id(Withdrawal),
    {ok, PostingsTransferEvents} = ff_postings_transfer:create(PTransferID, FinalCashFlow),
    {continue, [{p_transfer, Ev} || Ev <- PostingsTransferEvents]}.

-spec process_session_creation(withdrawal_state()) ->
    process_result().
process_session_creation(Withdrawal) ->
    ID = construct_session_id(Withdrawal),
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = params(Withdrawal),
    {ok, WalletMachine} = ff_wallet_machine:get(WalletID),
    Wallet = ff_wallet_machine:wallet(WalletMachine),
    WalletAccount = ff_wallet:account(Wallet),

    {ok, DestinationMachine} = ff_destination:get_machine(DestinationID),
    Destination = ff_destination:get(DestinationMachine),
    DestinationAccount = ff_destination:account(Destination),

    #{provider_id := ProviderID} = route(Withdrawal),
    {ok, SenderSt} = ff_identity_machine:get(ff_account:identity(WalletAccount)),
    {ok, ReceiverSt} = ff_identity_machine:get(ff_account:identity(DestinationAccount)),

    TransferData = genlib_map:compact(#{
        id          => ID,
        cash        => body(Withdrawal),
        sender      => ff_identity_machine:identity(SenderSt),
        receiver    => ff_identity_machine:identity(ReceiverSt),
        quote       => unwrap_quote(quote(Withdrawal))
    }),
    SessionParams = #{
        resource => destination_resource(Withdrawal),
        provider_id => ProviderID
    },
    ok = create_session(ID, TransferData, SessionParams),
    {continue, [{session_started, ID}]}.

-spec construct_session_id(withdrawal_state()) -> id().
construct_session_id(Withdrawal) ->
    ID = id(Withdrawal),
    Index = ff_withdrawal_route_utils:get_index(Withdrawal),
    SubID = integer_to_binary(Index),
    << ID/binary, "/", SubID/binary >>.

-spec construct_p_transfer_id(withdrawal_state()) -> id().
construct_p_transfer_id(Withdrawal) ->
    ID = id(Withdrawal),
    Index = ff_withdrawal_route_utils:get_index(Withdrawal),
    SubID = integer_to_binary(Index),
    <<"ff/withdrawal/", ID/binary, "/", SubID/binary >>.

create_session(ID, TransferData, SessionParams) ->
    case ff_withdrawal_session_machine:create(ID, TransferData, SessionParams) of
        ok ->
            ok;
        {error, exists} ->
            ok
    end.

-spec process_session_poll(withdrawal_state()) ->
    process_result().
process_session_poll(Withdrawal) ->
    SessionID = session_id(Withdrawal),
    {ok, SessionMachine} = ff_withdrawal_session_machine:get(SessionID),
    Session = ff_withdrawal_session_machine:session(SessionMachine),
    case ff_withdrawal_session:status(Session) of
        active ->
            {poll, []};
        {finished, Result} ->
            {continue, [{session_finished, {SessionID, Result}}]}
    end.

-spec process_transfer_finish(withdrawal_state()) ->
    process_result().
process_transfer_finish(_Withdrawal) ->
    {undefined, [{status_changed, succeeded}]}.

-spec process_transfer_fail(fail_type(), withdrawal_state()) ->
    process_result().
process_transfer_fail(FailType, Withdrawal) ->
    Failure = build_failure(FailType, Withdrawal),
    {undefined, [{status_changed, {failed, Failure}}]}.

-spec handle_child_result(process_result(), withdrawal_state()) -> process_result().
handle_child_result({undefined, Events} = Result, Withdrawal) ->
    NextWithdrawal = lists:foldl(fun(E, Acc) -> apply_event(E, Acc) end, Withdrawal, Events),
    case is_active(NextWithdrawal) of
        true ->
            {continue, Events};
        false ->
            Result
    end;
handle_child_result({_OtherAction, _Events} = Result, _Withdrawal) ->
    Result.

-spec is_childs_active(withdrawal_state()) -> boolean().
is_childs_active(Withdrawal) ->
    ff_adjustment_utils:is_active(adjustments_index(Withdrawal)).

-spec make_final_cash_flow(withdrawal_state()) ->
    final_cash_flow().
make_final_cash_flow(Withdrawal) ->
    Body = body(Withdrawal),
    WalletID = wallet_id(Withdrawal),
    {ok, Wallet} = get_wallet(WalletID),
    Route = route(Withdrawal),
    DomainRevision = operation_domain_revision(Withdrawal),
    {ok, Destination} = get_destination(destination_id(Withdrawal)),
    Resource = destination_resource(Withdrawal),
    Identity = get_wallet_identity(Wallet),
    PartyID = ff_identity:party(get_wallet_identity(Wallet)),
    PartyRevision = operation_party_revision(Withdrawal),
    ContractID = ff_identity:contract(Identity),
    Timestamp = operation_timestamp(Withdrawal),
    VarsetParams = genlib_map:compact(#{
        body => body(Withdrawal),
        wallet_id => WalletID,
        wallet => Wallet,
        party_id => PartyID,
        destination => Destination,
        resource => Resource
    }),
    PartyVarset = build_party_varset(VarsetParams),

    WalletAccount = ff_wallet:account(Wallet),
    DestinationAccount = ff_destination:account(Destination),

    {_Amount, CurrencyID} = Body,
    #{provider_id := ProviderID} = Route,
    {ok, Provider} = ff_payouts_provider:get(ProviderID),
    ProviderAccounts = ff_payouts_provider:accounts(Provider),
    ProviderAccount = maps:get(CurrencyID, ProviderAccounts, undefined),

    {ok, PaymentInstitutionID} = ff_party:get_identity_payment_institution_id(Identity),
    {ok, PaymentInstitution} = ff_payment_institution:get(PaymentInstitutionID, DomainRevision),
    {ok, SystemAccounts} = ff_payment_institution:compute_system_accounts(PaymentInstitution, PartyVarset),
    SystemAccount = maps:get(CurrencyID, SystemAccounts, #{}),
    SettlementAccount = maps:get(settlement, SystemAccount, undefined),
    SubagentAccount = maps:get(subagent, SystemAccount, undefined),

    ProviderFee = ff_payouts_provider:compute_fees(Provider, PartyVarset),

    {ok, Terms} = ff_party:get_contract_terms(
        PartyID, ContractID, PartyVarset, Timestamp, PartyRevision, DomainRevision
    ),
    {ok, WalletCashFlowPlan} = ff_party:get_withdrawal_cash_flow_plan(Terms),
    {ok, CashFlowPlan} = ff_cash_flow:add_fee(WalletCashFlowPlan, ProviderFee),
    Constants = #{
        operation_amount => Body
    },
    Accounts = genlib_map:compact(#{
        {wallet, sender_settlement} => WalletAccount,
        {wallet, receiver_destination} => DestinationAccount,
        {system, settlement} => SettlementAccount,
        {system, subagent} => SubagentAccount,
        {provider, settlement} => ProviderAccount
    }),
    {ok, FinalCashFlow} = ff_cash_flow:finalize(CashFlowPlan, Accounts, Constants),
    FinalCashFlow.

-spec ensure_domain_revision_defined(domain_revision() | undefined) ->
    domain_revision().
ensure_domain_revision_defined(undefined) ->
    ff_domain_config:head();
ensure_domain_revision_defined(Revision) ->
    Revision.

-spec ensure_party_revision_defined(party_id(), party_revision() | undefined) ->
    domain_revision().
ensure_party_revision_defined(PartyID, undefined) ->
    {ok, Revision} = ff_party:get_revision(PartyID),
    Revision;
ensure_party_revision_defined(_PartyID, Revision) ->
    Revision.

-spec get_wallet(wallet_id()) ->
    {ok, wallet()} | {error, notfound}.
get_wallet(WalletID) ->
    do(fun() ->
        WalletMachine = unwrap(ff_wallet_machine:get(WalletID)),
        ff_wallet_machine:wallet(WalletMachine)
    end).

-spec get_destination(destination_id()) ->
    {ok, destination()} | {error, notfound}.
get_destination(DestinationID) ->
    do(fun() ->
        DestinationMachine = unwrap(ff_destination:get_machine(DestinationID)),
        ff_destination:get(DestinationMachine)
    end).

-spec get_wallet_identity(wallet()) ->
    identity().
get_wallet_identity(Wallet) ->
    IdentityID = ff_wallet:identity(Wallet),
    {ok, IdentityMachine} = ff_identity_machine:get(IdentityID),
    ff_identity_machine:identity(IdentityMachine).

-spec build_party_varset(party_varset_params()) ->
    party_varset().
build_party_varset(#{body := Body, wallet_id := WalletID, party_id := PartyID} = Params) ->
    {_, CurrencyID} = Body,
    Destination = maps:get(destination, Params, undefined),
    Resource = maps:get(resource, Params, undefined),
    PaymentTool = case {Destination, Resource} of
        {undefined, _} ->
            undefined;
        {_, Resource} ->
            construct_payment_tool(Resource)
    end,
    genlib_map:compact(#{
        currency => ff_dmsl_codec:marshal(currency_ref, CurrencyID),
        cost => ff_dmsl_codec:marshal(cash, Body),
        party_id => PartyID,
        wallet_id => WalletID,
        payout_method => #domain_PayoutMethodRef{id = wallet_info},
        % TODO it's not fair, because it's PAYOUT not PAYMENT tool.
        payment_tool => PaymentTool
    }).

-spec construct_payment_tool(ff_destination:resource_full() | ff_destination:resource()) ->
    dmsl_domain_thrift:'PaymentTool'().
construct_payment_tool({bank_card, #{bank_card := ResourceBankCard}}) ->
    {bank_card, #domain_BankCard{
        token           = maps:get(token, ResourceBankCard),
        bin             = maps:get(bin, ResourceBankCard),
        last_digits     = maps:get(masked_pan, ResourceBankCard),
        payment_system  = maps:get(payment_system, ResourceBankCard),
        issuer_country  = maps:get(iso_country_code, ResourceBankCard, undefined),
        bank_name       = maps:get(bank_name, ResourceBankCard, undefined)
    }};

construct_payment_tool({crypto_wallet, #{crypto_wallet := #{currency := {Currency, _}}}}) ->
   {crypto_currency, Currency}.

%% Quote helpers

-spec get_quote(quote_params()) ->
    {ok, quote()} |
    {error,
        {destination, notfound}       |
        {destination, unauthorized}   |
        {route, route_not_found}      |
        {wallet, notfound}            |
        {destination_resource, {bin_data, not_found}}
    }.
get_quote(Params = #{destination_id := DestinationID}) ->
    do(fun() ->
        Destination = unwrap(destination, get_destination(DestinationID)),
        ok = unwrap(destination, valid(authorized, ff_destination:status(Destination))),
        Resource = unwrap(destination_resource, ff_destination:resource_full(Destination)),
        unwrap(get_quote_(Params, Destination, Resource))
    end);
get_quote(Params) ->
    get_quote_(Params, undefined, undefined).

get_quote_(Params, Destination, Resource) ->
    do(fun() ->
        #{
            wallet_id := WalletID,
            body := Body,
            currency_from := CurrencyFrom,
            currency_to := CurrencyTo
        } = Params,
        Timestamp = ff_time:now(),
        Wallet = unwrap(wallet, get_wallet(WalletID)),
        DomainRevision = ff_domain_config:head(),
        Identity = get_wallet_identity(Wallet),
        PartyID = ff_identity:party(Identity),
        {ok, PartyRevision} = ff_party:get_revision(PartyID),
        VarsetParams = genlib_map:compact(#{
            body => Body,
            wallet_id => WalletID,
            wallet => Wallet,
            party_id => PartyID,
            destination => Destination,
            resource => Resource
        }),
        [ProviderID | _] = unwrap(route, prepare_route(build_party_varset(VarsetParams), Identity, DomainRevision)),
        {Adapter, AdapterOpts} = ff_withdrawal_session:get_adapter_with_opts(ProviderID),
        GetQuoteParams = #{
            external_id => maps:get(external_id, Params, undefined),
            currency_from => CurrencyFrom,
            currency_to => CurrencyTo,
            body => Body
        },
        {ok, Quote} = ff_adapter_withdrawal:get_quote(Adapter, GetQuoteParams, AdapterOpts),
        %% add provider id to quote_data
        wrap_quote(DomainRevision, PartyRevision, Timestamp, Resource, ProviderID, Quote)
    end).

-spec wrap_quote(DomainRevision, PartyRevision, Timestamp, Resource, ProviderID, Quote) -> quote() when
    DomainRevision :: domain_revision(),
    PartyRevision :: party_revision(),
    Timestamp :: ff_time:timestamp_ms(),
    ProviderID :: provider_id(),
    Resource :: destination_resource() | undefined,
    Quote :: ff_adapter_withdrawal:quote().
wrap_quote(DomainRevision, PartyRevision, Timestamp, Resource, ProviderID, Quote) ->
    #{quote_data := QuoteData} = Quote,
    ResourceID = ff_destination:full_bank_card_id(Resource),
    Quote#{quote_data := genlib_map:compact(#{
        <<"version">> => 1,
        <<"quote_data">> => QuoteData,
        <<"provider_id">> => ProviderID,
        <<"resource_id">> => ResourceID,
        <<"timestamp">> => Timestamp,
        <<"domain_revision">> => DomainRevision,
        <<"party_revision">> => PartyRevision
    })}.

unwrap_quote(undefined) ->
    undefined;
unwrap_quote(Quote = #{quote_data := QuoteData}) ->
    WrappedData = maps:get(<<"quote_data">>, QuoteData),
    Quote#{quote_data := WrappedData}.

quote_resource_id(undefined) ->
    undefined;
quote_resource_id(#{quote_data := QuoteData}) ->
    maps:get(<<"resource_id">>, QuoteData, undefined).

-spec quote_timestamp(quote() | undefined) ->
    ff_time:timestamp_ms() | undefined.
quote_timestamp(undefined) ->
    undefined;
quote_timestamp(#{quote_data := QuoteData}) ->
    maps:get(<<"timestamp">>, QuoteData, undefined).

-spec quote_party_revision(quote() | undefined) ->
    party_revision() | undefined.
quote_party_revision(undefined) ->
    undefined;
quote_party_revision(#{quote_data := QuoteData}) ->
    maps:get(<<"party_revision">>, QuoteData, undefined).

-spec quote_domain_revision(quote() | undefined) ->
    domain_revision() | undefined.
quote_domain_revision(undefined) ->
    undefined;
quote_domain_revision(#{quote_data := QuoteData}) ->
    maps:get(<<"domain_revision">>, QuoteData, undefined).

%% Session management

-spec session(withdrawal_state()) -> session() | undefined.
session(Withdrawal) ->
    ff_withdrawal_route_utils:get_current_session(Withdrawal).

-spec session_id(withdrawal_state()) -> session_id() | undefined.
session_id(T) ->
    case session(T) of
        undefined ->
            undefined;
        #{id := SessionID} ->
            SessionID
    end.

-spec session_result(withdrawal_state()) -> session_result() | unknown | undefined.
session_result(Withdrawal) ->
    case session(Withdrawal) of
        undefined ->
            undefined;
        #{result := Result} ->
            Result;
        #{} ->
            unknown
    end.

-spec session_processing_status(withdrawal_state()) ->
    undefined | pending | succeeded | failed.
session_processing_status(Withdrawal) ->
    case routes(Withdrawal) of
        undefined ->
            undefined;
        _ ->
            session_processing_status_(Withdrawal)
    end.

session_processing_status_(Withdrawal) ->
    Session = session(Withdrawal),
    case Session of
        undefined ->
            undefined;
        #{result := {success, _}} ->
            succeeded;
        #{result := {failed, _}} ->
            failed;
        #{} ->
            pending
    end.

%% Withdrawal validators

-spec validate_withdrawal_creation(terms(), body(), wallet(), destination()) ->
    {ok, valid} |
    {error, create_error()}.
validate_withdrawal_creation(Terms, Body, Wallet, Destination) ->
    do(fun() ->
        valid = unwrap(terms, validate_withdrawal_creation_terms(Terms, Body)),
        valid = unwrap(validate_withdrawal_currency(Body, Wallet, Destination)),
        valid = unwrap(validate_destination_status(Destination))
    end).

-spec validate_withdrawal_creation_terms(terms(), body()) ->
    {ok, valid} |
    {error, ff_party:validate_withdrawal_creation_error()}.
validate_withdrawal_creation_terms(Terms, Body) ->
    ff_party:validate_withdrawal_creation(Terms, Body).

-spec validate_withdrawal_currency(body(), wallet(), destination()) ->
    {ok, valid} |
    {error, {inconsistent_currency, {currency_id(), currency_id(), currency_id()}}}.
validate_withdrawal_currency(Body, Wallet, Destination) ->
    DestiantionCurrencyID = ff_account:currency(ff_destination:account(Destination)),
    WalletCurrencyID = ff_account:currency(ff_wallet:account(Wallet)),
    case Body of
        {_Amount, WithdrawalCurencyID} when
            WithdrawalCurencyID =:= DestiantionCurrencyID andalso
            WithdrawalCurencyID =:= WalletCurrencyID
        ->
            {ok, valid};
        {_Amount, WithdrawalCurencyID} ->
            {error, {inconsistent_currency, {WithdrawalCurencyID, WalletCurrencyID, DestiantionCurrencyID}}}
    end.

-spec validate_destination_status(destination()) ->
    {ok, valid} |
    {error, {destinaiton, ff_destination:status()}}.
validate_destination_status(Destination) ->
    case ff_destination:status(Destination) of
        authorized ->
            {ok, valid};
        unauthorized ->
            {error, {destinaiton, unauthorized}}
    end.

%% Limit helpers

-spec add_limit_check(limit_check_details(), withdrawal_state()) ->
    withdrawal_state().
add_limit_check(Check, Withdrawal) ->
    Checks =
        case ff_withdrawal_route_utils:get_current_limit_checks(Withdrawal) of
            undefined ->
                [Check];
            C ->
                [Check | C]
        end,
    ff_withdrawal_route_utils:update_current_limit_checks(Checks, Withdrawal).

-spec limit_check_status(withdrawal_state()) ->
    ok | {failed, limit_check_details()} | unknown.
limit_check_status(Withdrawal) ->
    case routes(Withdrawal) of
        undefined ->
            unknown;
        _ ->
            Checks = ff_withdrawal_route_utils:get_current_limit_checks(Withdrawal),
            limit_check_status_(Checks)
    end.

limit_check_status_(undefined) ->
    unknown;
limit_check_status_(Checks) ->
    case lists:dropwhile(fun is_limit_check_ok/1, Checks) of
        [] ->
            ok;
        [H | _Tail] ->
            {failed, H}
    end.

-spec limit_check_processing_status(withdrawal_state()) ->
    ok | failed | unknown.
limit_check_processing_status(Withdrawal) ->
    case limit_check_status(Withdrawal) of
        ok ->
            ok;
        unknown ->
            unknown;
        {failed, _Details} ->
            failed
    end.

-spec is_limit_check_ok(limit_check_details()) -> boolean().
is_limit_check_ok({wallet_sender, ok}) ->
    true;
is_limit_check_ok({wallet_sender, {failed, _Details}}) ->
    false.

-spec validate_wallet_limits(terms(), wallet(), clock()) ->
    {ok, valid} |
    {error, {terms_violation, {wallet_limit, {cash_range, {cash(), cash_range()}}}}}.
validate_wallet_limits(Terms, Wallet, Clock) ->
    case ff_party:validate_wallet_limits(Terms, Wallet, Clock) of
        {ok, valid} = Result ->
            Result;
        {error, {terms_violation, {cash_range, {Cash, CashRange}}}} ->
            {error, {terms_violation, {wallet_limit, {cash_range, {Cash, CashRange}}}}};
        {error, {invalid_terms, _Details} = Reason} ->
            erlang:error(Reason)
    end.

%% Adjustment validators

-spec validate_adjustment_start(adjustment_params(), withdrawal_state()) ->
    {ok, valid} |
    {error, start_adjustment_error()}.
validate_adjustment_start(Params, Withdrawal) ->
    do(fun() ->
        valid = unwrap(validate_no_pending_adjustment(Withdrawal)),
        valid = unwrap(validate_withdrawal_finish(Withdrawal)),
        valid = unwrap(validate_status_change(Params, Withdrawal))
    end).

-spec validate_withdrawal_finish(withdrawal_state()) ->
    {ok, valid} |
    {error, {invalid_withdrawal_status, status()}}.
validate_withdrawal_finish(Withdrawal) ->
    case is_finished(Withdrawal) of
        true ->
            {ok, valid};
        false ->
            {error, {invalid_withdrawal_status, status(Withdrawal)}}
    end.

-spec validate_no_pending_adjustment(withdrawal_state()) ->
    {ok, valid} |
    {error, {another_adjustment_in_progress, adjustment_id()}}.
validate_no_pending_adjustment(Withdrawal) ->
    case ff_adjustment_utils:get_not_finished(adjustments_index(Withdrawal)) of
        error ->
            {ok, valid};
        {ok, AdjustmentID} ->
            {error, {another_adjustment_in_progress, AdjustmentID}}
    end.

-spec validate_status_change(adjustment_params(), withdrawal_state()) ->
    {ok, valid} |
    {error, invalid_status_change_error()}.
validate_status_change(#{change := {change_status, Status}}, Withdrawal) ->
    do(fun() ->
        valid = unwrap(invalid_status_change, validate_target_status(Status)),
        valid = unwrap(invalid_status_change, validate_change_same_status(Status, status(Withdrawal)))
    end);
validate_status_change(_Params, _Withdrawal) ->
    {ok, valid}.

-spec validate_target_status(status()) ->
    {ok, valid} |
    {error, {unavailable_status, status()}}.
validate_target_status(succeeded) ->
    {ok, valid};
validate_target_status({failed, _Failure}) ->
    {ok, valid};
validate_target_status(Status) ->
    {error, {unavailable_status, Status}}.

-spec validate_change_same_status(status(), status()) ->
    {ok, valid} |
    {error, {already_has_status, status()}}.
validate_change_same_status(NewStatus, OldStatus) when NewStatus =/= OldStatus ->
    {ok, valid};
validate_change_same_status(Status, Status) ->
    {error, {already_has_status, Status}}.

%% Adjustment helpers

-spec apply_adjustment_event(wrapped_adjustment_event(), withdrawal_state()) -> withdrawal_state().
apply_adjustment_event(WrappedEvent, Withdrawal) ->
    Adjustments0 = adjustments_index(Withdrawal),
    Adjustments1 = ff_adjustment_utils:apply_event(WrappedEvent, Adjustments0),
    set_adjustments_index(Adjustments1, Withdrawal).

-spec make_adjustment_params(adjustment_params(), withdrawal_state()) ->
    ff_adjustment:params().
make_adjustment_params(Params, Withdrawal) ->
    #{id := ID, change := Change} = Params,
    genlib_map:compact(#{
        id => ID,
        changes_plan => make_adjustment_change(Change, Withdrawal),
        external_id => genlib_map:get(external_id, Params),
        domain_revision => operation_domain_revision(Withdrawal),
        party_revision => operation_party_revision(Withdrawal),
        operation_timestamp => operation_timestamp(Withdrawal)
    }).

-spec make_adjustment_change(adjustment_change(), withdrawal_state()) ->
    ff_adjustment:changes().
make_adjustment_change({change_status, NewStatus}, Withdrawal) ->
    CurrentStatus = status(Withdrawal),
    make_change_status_params(CurrentStatus, NewStatus, Withdrawal).

-spec make_change_status_params(status(), status(), withdrawal_state()) ->
    ff_adjustment:changes().
make_change_status_params(succeeded, {failed, _} = NewStatus, Withdrawal) ->
    CurrentCashFlow = effective_final_cash_flow(Withdrawal),
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
make_change_status_params({failed, _}, succeeded = NewStatus, Withdrawal) ->
    CurrentCashFlow = effective_final_cash_flow(Withdrawal),
    NewCashFlow = make_final_cash_flow(Withdrawal),
    #{
        new_status => #{
            new_status => NewStatus
        },
        new_cash_flow => #{
            old_cash_flow_inverted => ff_cash_flow:inverse(CurrentCashFlow),
            new_cash_flow => NewCashFlow
        }
    };
make_change_status_params({failed, _}, {failed, _} = NewStatus, _Withdrawal) ->
    #{
        new_status => #{
            new_status => NewStatus
        }
    }.

-spec process_adjustment(withdrawal_state()) ->
    process_result().
process_adjustment(Withdrawal) ->
    #{
        action := Action,
        events := Events0,
        changes := Changes
    } = ff_adjustment_utils:process_adjustments(adjustments_index(Withdrawal)),
    Events1 = Events0 ++ handle_adjustment_changes(Changes),
    handle_child_result({Action, Events1}, Withdrawal).

-spec process_route_change(withdrawal_state()) ->
    [event()].
process_route_change(Withdrawal) ->
    Route = route(Withdrawal),
    [New | Rest] = maps:get(pending_providers, Route),
    [
        {route_changed, #{provider_id => New, pending_providers => Rest}},
        {status_changed, pending}
    ].

-spec handle_adjustment_changes(ff_adjustment:changes()) ->
    [event()].
handle_adjustment_changes(Changes) ->
    StatusChange = maps:get(new_status, Changes, undefined),
    handle_adjustment_status_change(StatusChange).

-spec handle_adjustment_status_change(ff_adjustment:status_change() | undefined) ->
    [event()].
handle_adjustment_status_change(undefined) ->
    [];
handle_adjustment_status_change(#{new_status := Status}) ->
    [{status_changed, Status}].

-spec save_adjustable_info(event(), withdrawal_state()) -> withdrawal_state().
save_adjustable_info({p_transfer, {status_changed, committed}}, Withdrawal) ->
    CashFlow = ff_postings_transfer:final_cash_flow(p_transfer(Withdrawal)),
    update_adjusment_index(fun ff_adjustment_utils:set_cash_flow/2, CashFlow, Withdrawal);
save_adjustable_info(_Ev, Withdrawal) ->
    Withdrawal.

-spec update_adjusment_index(Updater, Value, withdrawal_state()) -> withdrawal_state() when
    Updater :: fun((Value, adjustments_index()) -> adjustments_index()),
    Value :: any().
update_adjusment_index(Updater, Value, Withdrawal) ->
    Index = adjustments_index(Withdrawal),
    set_adjustments_index(Updater(Value, Index), Withdrawal).

%% Failure helpers

-spec build_failure(fail_type(), withdrawal_state()) -> failure().
build_failure(limit_check, Withdrawal) ->
    {failed, Details} = limit_check_status(Withdrawal),
    case Details of
        {wallet_sender, _WalletLimitDetails} ->
            #{
                code => <<"account_limit_exceeded">>,
                reason => genlib:format(Details),
                sub => #{
                    code => <<"amount">>
                }
            }
    end;
build_failure(route_not_found, _Withdrawal) ->
    #{
        code => <<"no_route_found">>
    };
build_failure({inconsistent_quote_route, FoundProviderID}, Withdrawal) ->
    #{quote_data := #{<<"provider_id">> := QuotaProviderID}} = quote(Withdrawal),
    Details = {inconsistent_quote_route, #{
        expected => QuotaProviderID,
        found => FoundProviderID
    }},
    #{
        code => <<"unknown">>,
        reason => genlib:format(Details)
    };
build_failure(session, Withdrawal) ->
    Result = session_result(Withdrawal),
    {failed, Failure} = Result,
    Failure.

%%

-spec apply_event(event() | legacy_event(), ff_maybe:maybe(withdrawal_state())) ->
    withdrawal_state().
apply_event(Ev, T0) ->
    T1 = apply_event_(Ev, T0),
    T2 = save_adjustable_info(Ev, T1),
    T2.

-spec apply_event_(event(), ff_maybe:maybe(withdrawal_state())) ->
    withdrawal_state().
apply_event_({created, T}, undefined) ->
    T;
apply_event_({status_changed, Status}, T) ->
    maps:put(status, Status, T);
apply_event_({resource_got, Resource}, T) ->
    maps:put(resource, Resource, T);
apply_event_({limit_check, Details}, T) ->
    add_limit_check(Details, T);
apply_event_({p_transfer, Ev}, T) ->
    Tr = ff_postings_transfer:apply_event(Ev, p_transfer(T)),
    ff_withdrawal_route_utils:update_current_p_transfer(Tr, T);
apply_event_({session_started, SessionID}, T) ->
    Session = #{id => SessionID},
    ff_withdrawal_route_utils:update_current_session(Session, T);
apply_event_({session_finished, {SessionID, Result}}, T) ->
    #{id := SessionID} = Session = ff_withdrawal_route_utils:get_current_session(T),
    UpdSession = Session#{result => Result},
    ff_withdrawal_route_utils:update_current_session(UpdSession, T);
apply_event_({route_changed, Route}, T) ->
    #{provider_id := PrID} = Route,
    T1 = maps:put(route, Route, T),
    ff_withdrawal_route_utils:new_route(PrID, T1);
apply_event_({adjustment, _Ev} = Event, T) ->
    apply_adjustment_event(Event, T).

-spec maybe_migrate(event() | legacy_event(), ff_machine:migrate_params()) ->
    event().
% Actual events
maybe_migrate(Ev = {created, #{version := ?ACTUAL_FORMAT_VERSION}}, _MigrateParams) ->
    Ev;
maybe_migrate(Ev = {status_changed, {failed, #{code := _}}}, _MigrateParams) ->
    Ev;
maybe_migrate(Ev = {session_finished, {_SessionID, _Status}}, _MigrateParams) ->
    Ev;
maybe_migrate(Ev = {limit_check, {wallet_sender, _Details}}, _MigrateParams) ->
    Ev;
maybe_migrate({p_transfer, PEvent}, _MigrateParams) ->
    {p_transfer, ff_postings_transfer:maybe_migrate(PEvent, withdrawal)};
maybe_migrate({adjustment, _Payload} = Event, _MigrateParams) ->
    ff_adjustment_utils:maybe_migrate(Event);
maybe_migrate({resource_got, Resource}, _MigrateParams) ->
    {resource_got, ff_instrument:maybe_migrate_resource(Resource)};

% Old events
maybe_migrate({limit_check, {wallet, Details}}, MigrateParams) ->
    maybe_migrate({limit_check, {wallet_sender, Details}}, MigrateParams);
maybe_migrate({created, #{version := 1, handler := ff_withdrawal} = T}, MigrateParams) ->
    #{
        version     := 1,
        id          := ID,
        handler     := ff_withdrawal,
        body        := Body,
        params      := #{
            destination := DestinationID,
            source      := SourceID
        }
    } = T,
    Route = maps:get(route, T, undefined),
    maybe_migrate({created, genlib_map:compact(#{
        version       => 2,
        id            => ID,
        transfer_type => withdrawal,
        body          => Body,
        route         => Route,
        params        => #{
            wallet_id             => SourceID,
            destination_id        => DestinationID,
            % Fields below are required to correctly decode legacy events.
            % When decoding legacy events, the `erlang:binary_to_existing_atom/2` function is used,
            % so the code must contain atoms from the event.
            % They are not used now, so their value does not matter.
            wallet_account        => [],
            destination_account   => [],
            wallet_cash_flow_plan => []
        }
    })}, MigrateParams);
maybe_migrate({created, Withdrawal = #{version := 2, id := ID}}, MigrateParams) ->
    Ctx = maps:get(ctx, MigrateParams, undefined),
    Context = case Ctx of
        undefined ->
            {ok, State} = ff_machine:get(ff_withdrawal, 'ff/withdrawal_v2', ID, {undefined, 0, forward}),
            maps:get(ctx, State, undefined);
        Data ->
            Data
    end,
    maybe_migrate({created, genlib_map:compact(Withdrawal#{
        version => 3,
        metadata => ff_entity_context:try_get_legacy_metadata(Context)
    })}, MigrateParams);
maybe_migrate({created, T}, MigrateParams) ->
    DestinationID = maps:get(destination, T),
    SourceID = maps:get(source, T),
    ProviderID = maps:get(provider, T),
    maybe_migrate({created, T#{
        version     => 1,
        handler     => ff_withdrawal,
        route       => #{provider_id => ProviderID},
        params => #{
            destination => DestinationID,
            source      => SourceID
        }
    }}, MigrateParams);
maybe_migrate({transfer, PTransferEv}, MigrateParams) ->
    maybe_migrate({p_transfer, PTransferEv}, MigrateParams);
maybe_migrate({status_changed, {failed, LegacyFailure}}, MigrateParams) ->
    Failure = #{
        code => <<"unknown">>,
        reason => genlib:format(LegacyFailure)
    },
    maybe_migrate({status_changed, {failed, Failure}}, MigrateParams);
maybe_migrate({session_finished, SessionID}, MigrateParams) ->
    {ok, SessionMachine} = ff_withdrawal_session_machine:get(SessionID),
    Session = ff_withdrawal_session_machine:session(SessionMachine),
    {finished, Result} = ff_withdrawal_session:status(Session),
    maybe_migrate({session_finished, {SessionID, Result}}, MigrateParams);
% Other events
maybe_migrate(Ev, _MigrateParams) ->
    Ev.

%% Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> _.

-spec v0_created_migration_test() -> _.
v0_created_migration_test() ->
    ID = genlib:unique(),
    WalletID = genlib:unique(),
    DestinationID = genlib:unique(),
    ProviderID = genlib:unique(),
    Body = {100, <<"RUB">>},
    LegacyEvent = {created, #{
        id          => ID,
        source      => WalletID,
        destination => DestinationID,
        body        => Body,
        provider    => ProviderID
    }},
    {created, Withdrawal} = maybe_migrate(LegacyEvent, #{
        ctx => #{
            <<"com.rbkmoney.wapi">> => #{
                <<"metadata">> => #{
                    <<"some key">> => <<"some val">>
                }
            }
        }
    }),
    ?assertEqual(ID, id(Withdrawal)),
    ?assertEqual(WalletID, wallet_id(Withdrawal)),
    ?assertEqual(DestinationID, destination_id(Withdrawal)),
    ?assertEqual(Body, body(Withdrawal)),
    ?assertEqual(#{provider_id => ProviderID}, route(Withdrawal)).

-spec v1_created_migration_test() -> _.
v1_created_migration_test() ->
    ID = genlib:unique(),
    WalletID = genlib:unique(),
    WalletAccount = #{
        id => WalletID,
        identity => genlib:unique(),
        currency => <<"RUB">>,
        accounter_account_id => 123
    },
    DestinationID = genlib:unique(),
    DestinationAccount = #{
        id => DestinationID,
        identity => genlib:unique(),
        currency => <<"RUB">>,
        accounter_account_id => 123
    },
    Body = {100, <<"RUB">>},
    LegacyEvent = {created, #{
        version     => 1,
        id          => ID,
        handler     => ff_withdrawal,
        source      => WalletAccount,
        destination => DestinationAccount,
        body        => Body,
        params      => #{
            source => WalletID,
            destination => DestinationID
        }
    }},
    {created, Withdrawal} = maybe_migrate(LegacyEvent, #{
        ctx => #{
            <<"com.rbkmoney.wapi">> => #{
                <<"metadata">> => #{
                    <<"some key">> => <<"some val">>
                }
            }
        }
    }),
    ?assertEqual(ID, id(Withdrawal)),
    ?assertEqual(WalletID, wallet_id(Withdrawal)),
    ?assertEqual(DestinationID, destination_id(Withdrawal)),
    ?assertEqual(Body, body(Withdrawal)).

-spec v2_created_migration_test() -> _.
v2_created_migration_test() ->
    ID = genlib:unique(),
    WalletID = genlib:unique(),
    WalletAccount = #{
        id => WalletID,
        identity => genlib:unique(),
        currency => <<"RUB">>,
        accounter_account_id => 123
    },
    DestinationID = genlib:unique(),
    DestinationAccount = #{
        id => DestinationID,
        identity => genlib:unique(),
        currency => <<"RUB">>,
        accounter_account_id => 123
    },
    Body = {100, <<"RUB">>},
    LegacyEvent = {created, #{
        version       => 2,
        id            => ID,
        transfer_type => withdrawal,
        body          => Body,
        params        => #{
            wallet_id             => WalletID,
            destination_id        => DestinationID,
            wallet_account        => WalletAccount,
            destination_account   => DestinationAccount,
            wallet_cash_flow_plan => #{
                postings => [
                    #{
                        sender   => {wallet, sender_settlement},
                        receiver => {wallet, receiver_destination},
                        volume   => {share, {{1, 1}, operation_amount, default}}
                    }
                ]
            }
        }
    }},
    {created, Withdrawal} = maybe_migrate(LegacyEvent, #{
        ctx => #{
            <<"com.rbkmoney.wapi">> => #{
                <<"metadata">> => #{
                    <<"some key">> => <<"some val">>
                }
            }
        }
    }),
    ?assertEqual(ID, id(Withdrawal)),
    ?assertEqual(WalletID, wallet_id(Withdrawal)),
    ?assertEqual(DestinationID, destination_id(Withdrawal)),
    ?assertEqual(Body, body(Withdrawal)).

-spec v3_created_migration_test() -> _.
v3_created_migration_test() ->
    ID = genlib:unique(),
    WalletID = genlib:unique(),
    WalletAccount = #{
        id => WalletID,
        identity => genlib:unique(),
        currency => <<"RUB">>,
        accounter_account_id => 123
    },
    DestinationID = genlib:unique(),
    DestinationAccount = #{
        id => DestinationID,
        identity => genlib:unique(),
        currency => <<"RUB">>,
        accounter_account_id => 123
    },
    Body = {100, <<"RUB">>},
    LegacyEvent = {created, #{
        version       => 2,
        id            => ID,
        transfer_type => withdrawal,
        body          => Body,
        params        => #{
            wallet_id             => WalletID,
            destination_id        => DestinationID,
            wallet_account        => WalletAccount,
            destination_account   => DestinationAccount,
            wallet_cash_flow_plan => #{
                postings => [
                    #{
                        sender   => {wallet, sender_settlement},
                        receiver => {wallet, receiver_destination},
                        volume   => {share, {{1, 1}, operation_amount, default}}
                    }
                ]
            }
        }
    }},
    {created, Withdrawal} = maybe_migrate(LegacyEvent, #{
        ctx => #{
            <<"com.rbkmoney.wapi">> => #{
                <<"metadata">> => #{
                    <<"some key">> => <<"some val">>
                }
            }
        }
    }),
    ?assertEqual(ID, id(Withdrawal)),
    ?assertEqual(#{<<"some key">> => <<"some val">>}, metadata(Withdrawal)).

-endif.
