%%%
%%% Withdrawal
%%%

-module(ff_withdrawal).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_withdrawals_provider_adapter_thrift.hrl").

-type id() :: binary().
-type clock() :: ff_transaction:clock().

-define(ACTUAL_FORMAT_VERSION, 4).
-opaque withdrawal_state() :: #{
    id              := id(),
    transfer_type   := withdrawal,
    body            := body(),
    params          := transfer_params(),
    created_at      => ff_time:timestamp_ms(),
    party_revision  => party_revision(),
    domain_revision => domain_revision(),
    route           => route(),
    attempts        => attempts(),
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
    {wallet, ff_wallet:inaccessibility()}  |
    {inconsistent_currency, {Withdrawal :: currency_id(), Wallet :: currency_id(), Destination :: currency_id()}} |
    {terms, ff_party:validate_withdrawal_creation_error()} |
    {identity_providers_mismatch, {ff_provider:id(), ff_provider:id()}} |
    {destination_resource, {bin_data, ff_bin_data:bin_data_error()}}.

-type route() :: ff_withdrawal_routing:route().

-type attempts() :: ff_withdrawal_route_attempt_utils:attempts().

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

-type quote() :: #{
    cash_from := cash(),
    cash_to := cash(),
    created_at := binary(),
    expires_on := binary(),
    quote_data := ff_adapter_withdrawal:quote(),
    route := route(),
    operation_timestamp := ff_time:timestamp_ms(),
    resource_descriptor => resource_descriptor(),
    domain_revision => party_revision(),
    party_revision => domain_revision()
}.

-type quote_state() :: #{
    cash_from := cash(),
    cash_to := cash(),
    created_at := binary(),
    expires_on := binary(),
    quote_data := ff_adapter_withdrawal:quote(),
    route := route(),
    resource_descriptor => resource_descriptor()
}.

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

-type action() :: sleep | continue | undefined.

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

%%

-export([process_session_finished/3]).

%% Accessors

-export([wallet_id/1]).
-export([destination_id/1]).
-export([quote/1]).
-export([id/1]).
-export([body/1]).
-export([status/1]).
-export([route/1]).
-export([attempts/1]).
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
-export([get_current_session/1]).
-export([get_current_session_status/1]).

%% Event source

-export([apply_event/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

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
-type resource_descriptor()   :: ff_destination:resource_id().

-type wrapped_adjustment_event()  :: ff_adjustment_utils:wrapped_event().

-type provider_id() :: ff_payouts_provider:id().
-type terminal_id() :: ff_payouts_terminal:id().

-type legacy_event() :: any().

-type transfer_params() :: #{
    wallet_id      := wallet_id(),
    destination_id := destination_id(),
    quote          => quote_state()
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
    session_sleeping |
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
    {inconsistent_quote_route, {provider_id, provider_id()} | {terminal_id, terminal_id()}} |
    session.

-type session_processing_status() :: undefined | pending | succeeded | failed.

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
    {ok, DestinationMachine} = ff_destination_machine:get(DestinationID),
    Destination = ff_destination_machine:destination(DestinationMachine),
    {ok, Resource} = ff_destination:resource_full(Destination),
    Resource.

%%

-spec quote(withdrawal_state()) -> quote_state() | undefined.
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

-spec attempts(withdrawal_state()) -> attempts() | undefined.
attempts(T) ->
    maps:get(attempts, T, undefined).

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
    Withdrawal = genlib_map:compact(maps:with(TypeKeys, Args)),
    Withdrawal#{version => 4}.

-spec create(params()) ->
    {ok, [event()]} |
    {error, create_error()}.
create(Params) ->
    do(fun() ->
        #{id := ID, wallet_id := WalletID, destination_id := DestinationID, body := Body} = Params,
        CreatedAt = ff_time:now(),
        Quote = maps:get(quote, Params, undefined),
        ResourceDescriptor = quote_resource_descriptor(Quote),
        Timestamp = ff_maybe:get_defined(quote_timestamp(Quote), CreatedAt),
        DomainRevision = ensure_domain_revision_defined(quote_domain_revision(Quote)),
        Wallet = unwrap(wallet, get_wallet(WalletID)),
        accessible = unwrap(wallet, ff_wallet:is_accessible(Wallet)),
        Destination = unwrap(destination, get_destination(DestinationID)),
        Resource = unwrap(destination_resource, ff_destination:resource_full(Destination, ResourceDescriptor)),

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
    ff_withdrawal_route_attempt_utils:get_sessions(attempts(Withdrawal)).

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

%%

-spec process_session_finished(session_id(), session_result(), withdrawal_state()) ->
    {ok, process_result()} | {error, session_not_found | old_session | result_mismatch}.
process_session_finished(SessionID, SessionResult, Withdrawal) ->
    case get_session_by_id(SessionID, Withdrawal) of
        #{id := SessionID, result := SessionResult} ->
            {ok, {undefined, []}};
        #{id := SessionID, result := _OtherSessionResult} ->
            {error, result_mismatch};
        #{id := SessionID} ->
            try_finish_session(SessionID, SessionResult, Withdrawal);
        undefined ->
            {error, session_not_found}
    end.

-spec get_session_by_id(session_id(), withdrawal_state()) ->
    session() | undefined.
get_session_by_id(SessionID, Withdrawal) ->
    Sessions = ff_withdrawal_route_attempt_utils:get_sessions(attempts(Withdrawal)),
    case lists:filter(fun(#{id := SessionID0}) -> SessionID0 =:= SessionID end, Sessions) of
        [Session] -> Session;
        [] -> undefined
    end.

-spec try_finish_session(session_id(), session_result(), withdrawal_state()) ->
    {ok, process_result()} | {error, old_session}.
try_finish_session(SessionID, SessionResult, Withdrawal) ->
    case is_current_session(SessionID, Withdrawal) of
        true ->
            {ok, {continue, [{session_finished, {SessionID, SessionResult}}]}};
        false ->
            {error, old_session}
    end.

-spec is_current_session(session_id(), withdrawal_state()) ->
    boolean().
is_current_session(SessionID, Withdrawal) ->
    case session_id(Withdrawal) of
        SessionID ->
            true;
        _ ->
            false
    end.

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

-spec update_attempts(attempts(), withdrawal_state()) -> withdrawal_state().
update_attempts(Attempts, T) ->
    maps:put(attempts, Attempts, T).

-spec params(withdrawal_state()) -> transfer_params().
params(#{params := V}) ->
    V.

-spec p_transfer(withdrawal_state()) -> p_transfer() | undefined.
p_transfer(Withdrawal) ->
    ff_withdrawal_route_attempt_utils:get_current_p_transfer(attempts(Withdrawal)).

-spec p_transfer_status(withdrawal_state()) -> ff_postings_transfer:status() | undefined.
p_transfer_status(Withdrawal) ->
    case attempts(Withdrawal) of
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
        session => get_current_session_status(Withdrawal),
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
    session_sleeping;
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
    Tr = ff_withdrawal_route_attempt_utils:get_current_p_transfer(attempts(Withdrawal)),
    {ok, Events} = ff_postings_transfer:prepare(Tr),
    {continue, [{p_transfer, Ev} || Ev <- Events]};
do_process_transfer(p_transfer_commit, Withdrawal) ->
    Tr = ff_withdrawal_route_attempt_utils:get_current_p_transfer(attempts(Withdrawal)),
    {ok, Events} = ff_postings_transfer:commit(Tr),
    {continue, [{p_transfer, Ev} || Ev <- Events]};
do_process_transfer(p_transfer_cancel, Withdrawal) ->
    Tr = ff_withdrawal_route_attempt_utils:get_current_p_transfer(attempts(Withdrawal)),
    {ok, Events} = ff_postings_transfer:cancel(Tr),
    {continue, [{p_transfer, Ev} || Ev <- Events]};
do_process_transfer(limit_check, Withdrawal) ->
    process_limit_check(Withdrawal);
do_process_transfer(session_starting, Withdrawal) ->
    process_session_creation(Withdrawal);
do_process_transfer(session_sleeping, Withdrawal) ->
    process_session_sleep(Withdrawal);
do_process_transfer({fail, Reason}, Withdrawal) ->
    {ok, Providers} = do_process_routing(Withdrawal),
    process_route_change(Providers, Withdrawal, Reason);
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
        {ok, [Route | _]} ->
            {continue, [
                {route_changed, Route}
            ]};
        {error, route_not_found} ->
            process_transfer_fail(route_not_found, Withdrawal);
        {error, {inconsistent_quote_route, _Data} = Reason} ->
            process_transfer_fail(Reason, Withdrawal)
    end.

-spec do_process_routing(withdrawal_state()) -> {ok, [route()]} | {error, Reason} when
    Reason :: route_not_found | InconsistentQuote,
    InconsistentQuote :: {inconsistent_quote_route, {provider_id, provider_id()} | {terminal_id, terminal_id()}}.
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
        Routes = unwrap(prepare_route(build_party_varset(VarsetParams), Identity, DomainRevision)),
        case quote(Withdrawal) of
            undefined ->
                Routes;
            Quote ->
                Route = hd(Routes),
                valid = unwrap(validate_quote_route(Route, Quote)),
                [Route]
        end
    end).

-spec prepare_route(party_varset(), identity(), domain_revision()) ->
    {ok, [route()]} | {error, route_not_found}.

prepare_route(PartyVarset, Identity, DomainRevision) ->
    ff_withdrawal_routing:prepare_routes(PartyVarset, Identity, DomainRevision).

-spec validate_quote_route(route(), quote_state()) -> {ok, valid} | {error, InconsistentQuote} when
    InconsistentQuote :: {inconsistent_quote_route, {provider_id, provider_id()} | {terminal_id, terminal_id()}}.

validate_quote_route(Route, #{route := QuoteRoute}) ->
    do(fun() ->
        valid = unwrap(validate_quote_provider(Route, QuoteRoute)),
        valid = unwrap(validate_quote_terminal(Route, QuoteRoute))
    end).

validate_quote_provider(#{provider_id := ProviderID}, #{provider_id := ProviderID}) ->
    {ok, valid};
validate_quote_provider(#{provider_id := ProviderID}, _) ->
    {error, {inconsistent_quote_route, {provider_id, ProviderID}}}.

validate_quote_terminal(#{terminal_id := TerminalID}, #{terminal_id := TerminalID}) ->
    {ok, valid};
validate_quote_terminal(#{terminal_id := TerminalID}, _) ->
    {error, {inconsistent_quote_route, {terminal_id, TerminalID}}}.

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

    {ok, DestinationMachine} = ff_destination_machine:get(DestinationID),
    Destination = ff_destination_machine:destination(DestinationMachine),
    DestinationAccount = ff_destination:account(Destination),

    Route = route(Withdrawal),
    {ok, SenderSt} = ff_identity_machine:get(ff_account:identity(WalletAccount)),
    {ok, ReceiverSt} = ff_identity_machine:get(ff_account:identity(DestinationAccount)),

    TransferData = genlib_map:compact(#{
        id          => ID,
        cash        => body(Withdrawal),
        sender      => ff_identity_machine:identity(SenderSt),
        receiver    => ff_identity_machine:identity(ReceiverSt),
        quote       => build_session_quote(quote(Withdrawal))
    }),
    SessionParams = #{
        withdrawal_id => id(Withdrawal),
        resource => destination_resource(Withdrawal),
        route => Route
    },
    ok = create_session(ID, TransferData, SessionParams),
    {continue, [{session_started, ID}]}.

-spec construct_session_id(withdrawal_state()) -> id().
construct_session_id(Withdrawal) ->
    ID = id(Withdrawal),
    Attempt = ff_withdrawal_route_attempt_utils:get_attempt(attempts(Withdrawal)),
    SubID = integer_to_binary(Attempt),
    << ID/binary, "/", SubID/binary >>.

-spec construct_p_transfer_id(withdrawal_state()) -> id().
construct_p_transfer_id(Withdrawal) ->
    ID = id(Withdrawal),
    Attempt = ff_withdrawal_route_attempt_utils:get_attempt(attempts(Withdrawal)),
    SubID = integer_to_binary(Attempt),
    <<"ff/withdrawal/", ID/binary, "/", SubID/binary >>.

create_session(ID, TransferData, SessionParams) ->
    case ff_withdrawal_session_machine:create(ID, TransferData, SessionParams) of
        ok ->
            ok;
        {error, exists} ->
            ok
    end.

-spec process_session_sleep(withdrawal_state()) ->
    process_result().
process_session_sleep(Withdrawal) ->
    SessionID = session_id(Withdrawal),
    {ok, SessionMachine} = ff_withdrawal_session_machine:get(SessionID),
    Session = ff_withdrawal_session_machine:session(SessionMachine),
    case ff_withdrawal_session:status(Session) of
        active ->
            {sleep, []};
        {finished, _} ->
            Result = ff_withdrawal_session:result(Session),
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

    {ok, ProviderFee} = ff_payouts_provider:compute_fees(Provider, PartyVarset),

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
        DestinationMachine = unwrap(ff_destination_machine:get(DestinationID)),
        ff_destination_machine:destination(DestinationMachine)
    end).

-spec get_wallet_identity(wallet()) ->
    identity().
get_wallet_identity(Wallet) ->
    IdentityID = ff_wallet:identity(Wallet),
    get_identity(IdentityID).

-spec get_destination_identity(destination()) ->
    identity().
get_destination_identity(Destination) ->
    IdentityID = ff_destination:identity(Destination),
    get_identity(IdentityID).

get_identity(IdentityID) ->
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
        create_error() |
        {route, route_not_found}
    }.
get_quote(Params = #{destination_id := DestinationID, body := Body, wallet_id := WalletID}) ->
    do(fun() ->
        Destination = unwrap(destination, get_destination(DestinationID)),
        Resource = unwrap(destination_resource, ff_destination:resource_full(Destination)),
        Wallet = unwrap(wallet, get_wallet(WalletID)),
        Identity = get_wallet_identity(Wallet),
        ContractID = ff_identity:contract(Identity),
        PartyID = ff_identity:party(Identity),
        DomainRevision = ff_domain_config:head(),
        {ok, PartyRevision} = ff_party:get_revision(PartyID),
        VarsetParams = genlib_map:compact(#{
            body => Body,
            wallet_id => WalletID,
            wallet => Wallet,
            party_id => PartyID,
            destination => Destination,
            resource => Resource
        }),
        PartyVarset = build_party_varset(VarsetParams),
        Timestamp = ff_time:now(),
        {ok, Terms} = ff_party:get_contract_terms(
            PartyID, ContractID, PartyVarset, Timestamp, PartyRevision, DomainRevision
        ),
        valid = unwrap(validate_withdrawal_creation(Terms, Body, Wallet, Destination)),
        GetQuoteParams = #{
            base_params => Params,
            identity => Identity,
            party_varset => build_party_varset(VarsetParams),
            timestamp => Timestamp,
            domain_revision => DomainRevision,
            party_revision => PartyRevision,
            resource => Resource
        },
        unwrap(get_quote_(GetQuoteParams))
    end);
get_quote(Params) ->
    #{
        wallet_id := WalletID,
        body := Body
    } = Params,
    Wallet = unwrap(wallet, get_wallet(WalletID)),
    Identity = get_wallet_identity(Wallet),
    PartyID = ff_identity:party(Identity),
    Timestamp = ff_time:now(),
    DomainRevision = ff_domain_config:head(),
    {ok, PartyRevision} = ff_party:get_revision(PartyID),
    VarsetParams = genlib_map:compact(#{
        body => Body,
        wallet_id => WalletID,
        wallet => Wallet,
        party_id => PartyID
    }),
    GetQuoteParams = #{
        base_params => Params,
        identity => Identity,
        party_varset => build_party_varset(VarsetParams),
        timestamp => Timestamp,
        domain_revision => DomainRevision,
        party_revision => PartyRevision
    },
    get_quote_(GetQuoteParams).

get_quote_(Params) ->
    do(fun() ->
        #{
            base_params := #{
                body := Body,
                currency_from := CurrencyFrom,
                currency_to := CurrencyTo
            },
            identity := Identity,
            party_varset := Varset,
            timestamp := Timestamp,
            domain_revision := DomainRevision,
            party_revision := PartyRevision
        } = Params,
        Resource = maps:get(resource, Params, undefined),

        [Route | _] = unwrap(route, prepare_route(Varset, Identity, DomainRevision)),
        {Adapter, AdapterOpts} = ff_withdrawal_session:get_adapter_with_opts(Route),
        GetQuoteParams = #{
            external_id => maps:get(external_id, Params, undefined),
            currency_from => CurrencyFrom,
            currency_to => CurrencyTo,
            body => Body
        },
        {ok, Quote} = ff_adapter_withdrawal:get_quote(Adapter, GetQuoteParams, AdapterOpts),
        genlib_map:compact(#{
            cash_from => maps:get(cash_from, Quote),
            cash_to => maps:get(cash_to, Quote),
            created_at => maps:get(created_at, Quote),
            expires_on => maps:get(expires_on, Quote),
            quote_data => maps:get(quote_data, Quote),
            route => Route,
            operation_timestamp => Timestamp,
            resource_descriptor => ff_destination:resource_id(Resource),
            domain_revision => DomainRevision,
            party_revision => PartyRevision
        })
    end).

-spec build_session_quote(quote_state() | undefined) ->
    ff_adapter_withdrawal:quote_data() | undefined.
build_session_quote(undefined) ->
    undefined;
build_session_quote(Quote) ->
    maps:with([cash_from, cash_to, created_at, expires_on, quote_data], Quote).

-spec quote_resource_descriptor(quote() | undefined) ->
    resource_descriptor().
quote_resource_descriptor(undefined) ->
    undefined;
quote_resource_descriptor(Quote) ->
    maps:get(resource_descriptor, Quote, undefined).

-spec quote_timestamp(quote() | undefined) ->
    ff_time:timestamp_ms() | undefined.
quote_timestamp(undefined) ->
    undefined;
quote_timestamp(Quote) ->
    maps:get(operation_timestamp, Quote, undefined).

-spec quote_party_revision(quote() | undefined) ->
    party_revision() | undefined.
quote_party_revision(undefined) ->
    undefined;
quote_party_revision(Quote) ->
    maps:get(party_revision, Quote, undefined).

-spec quote_domain_revision(quote() | undefined) ->
    domain_revision() | undefined.
quote_domain_revision(undefined) ->
    undefined;
quote_domain_revision(Quote) ->
    maps:get(domain_revision, Quote, undefined).

%% Session management

-spec get_current_session(withdrawal_state()) -> session() | undefined.
get_current_session(Withdrawal) ->
    ff_withdrawal_route_attempt_utils:get_current_session(attempts(Withdrawal)).

-spec session_id(withdrawal_state()) -> session_id() | undefined.
session_id(T) ->
    case get_current_session(T) of
        undefined ->
            undefined;
        #{id := SessionID} ->
            SessionID
    end.

-spec get_session_result(withdrawal_state()) -> session_result() | unknown | undefined.
get_session_result(Withdrawal) ->
    case get_current_session(Withdrawal) of
        undefined ->
            undefined;
        #{result := Result} ->
            Result;
        #{} ->
            unknown
    end.

-spec get_current_session_status(withdrawal_state()) ->
    session_processing_status().
get_current_session_status(Withdrawal) ->
    case attempts(Withdrawal) of
        undefined ->
            undefined;
        _ ->
            get_current_session_status_(Withdrawal)
    end.

get_current_session_status_(Withdrawal) ->
    Session = get_current_session(Withdrawal),
    case Session of
        undefined ->
            undefined;
        #{result := success} ->
            succeeded;
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
        valid = unwrap(validate_destination_status(Destination)),
        valid = unwrap(validate_withdrawal_providers(Wallet, Destination))
    end).

validate_withdrawal_providers(Wallet, Destination) ->
    WalletIdentity = get_wallet_identity(Wallet),
    DestinationIdentity = get_destination_identity(Destination),
    WalletProvider = ff_identity:provider(WalletIdentity),
    DestinationProvider = ff_identity:provider(DestinationIdentity),
    case WalletProvider =:= DestinationProvider of
        true  -> {ok, valid};
        false -> {error, {identity_providers_mismatch, {WalletProvider, DestinationProvider}}}
    end.

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
    Attempts = attempts(Withdrawal),
    Checks =
        case ff_withdrawal_route_attempt_utils:get_current_limit_checks(Attempts) of
            undefined ->
                [Check];
            C ->
                [Check | C]
        end,
    R = ff_withdrawal_route_attempt_utils:update_current_limit_checks(Checks, Attempts),
    update_attempts(R, Withdrawal).

-spec limit_check_status(withdrawal_state()) ->
    ok | {failed, limit_check_details()} | unknown.
limit_check_status(Withdrawal) ->
    Attempts = attempts(Withdrawal),
    Checks = ff_withdrawal_route_attempt_utils:get_current_limit_checks(Attempts),
    limit_check_status_(Checks).

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

-spec process_route_change([route()], withdrawal_state(), fail_type()) ->
    process_result().
process_route_change(Providers, Withdrawal, Reason) ->
    case is_failure_transient(Reason, Withdrawal) of
        true ->
            do_process_route_change(Providers, Withdrawal, Reason);
        false ->
            process_transfer_fail(Reason, Withdrawal)
    end.

-spec is_failure_transient(fail_type(), withdrawal_state()) ->
    boolean().
is_failure_transient(Reason, Withdrawal) ->
    {ok, Wallet} = get_wallet(wallet_id(Withdrawal)),
    PartyID = ff_identity:party(get_wallet_identity(Wallet)),
    RetryableErrors = get_retryable_error_list(PartyID),
    ErrorTokens = to_error_token_list(Reason, Withdrawal),
    match_error_whitelist(ErrorTokens, RetryableErrors).

-spec get_retryable_error_list(party_id()) ->
    list(list(binary())).
get_retryable_error_list(PartyID) ->
    WithdrawalConfig = genlib_app:env(ff_transfer, withdrawal, #{}),
    PartyRetryableErrors = maps:get(party_transient_errors, WithdrawalConfig, #{}),
    Errors = case maps:get(PartyID, PartyRetryableErrors, undefined) of
        undefined ->
            maps:get(default_transient_errors, WithdrawalConfig, []);
        ErrorList ->
            ErrorList
    end,
    binaries_to_error_tokens(Errors).

-spec binaries_to_error_tokens(list(binary())) ->
    list(list(binary())).
binaries_to_error_tokens(Errors) ->
    lists:map(fun(Error) ->
        binary:split(Error, <<":">>, [global])
    end, Errors).

-spec to_error_token_list(fail_type(), withdrawal_state()) ->
    list(binary()).
to_error_token_list(Reason, Withdrawal) ->
    Failure = build_failure(Reason, Withdrawal),
    failure_to_error_token_list(Failure).

-spec failure_to_error_token_list(ff_failure:failure()) ->
    list(binary()).
failure_to_error_token_list(#{code := Code, sub := SubFailure}) ->
    SubFailureList = failure_to_error_token_list(SubFailure),
    [Code | SubFailureList];
failure_to_error_token_list(#{code := Code}) ->
    [Code].

-spec match_error_whitelist(list(binary()), list(list(binary()))) ->
    boolean().
match_error_whitelist(ErrorTokens, RetryableErrors) ->
    lists:any(fun(RetryableError) ->
        error_tokens_match(ErrorTokens, RetryableError)
    end, RetryableErrors).

-spec error_tokens_match(list(binary()), list(binary())) ->
    boolean().
error_tokens_match(_, []) ->
    true;
error_tokens_match([], [_|_]) ->
    false;
error_tokens_match([Token0 | Rest0], [Token1 | Rest1]) when Token0 =:= Token1 ->
    error_tokens_match(Rest0, Rest1);
error_tokens_match([Token0 | _], [Token1 | _]) when Token0 =/= Token1 ->
    false.

-spec do_process_route_change([route()], withdrawal_state(), fail_type()) ->
    process_result().
do_process_route_change(Routes, Withdrawal, Reason) ->
    Attempts = attempts(Withdrawal),
    AttemptLimit = get_attempt_limit(Withdrawal),
    case ff_withdrawal_route_attempt_utils:next_route(Routes, Attempts, AttemptLimit) of
        {ok, Route} ->
            {continue, [
                {route_changed, Route}
            ]};
        {error, route_not_found} ->
            %% No more routes, return last error
            process_transfer_fail(Reason, Withdrawal);
        {error, attempt_limit_exceeded} ->
            %% Attempt limit exceeded, return last error
            process_transfer_fail(Reason, Withdrawal)
    end.

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
build_failure({inconsistent_quote_route, {Type, FoundID}}, Withdrawal) ->
    Details = {inconsistent_quote_route, #{
        expected => {Type, FoundID},
        found => get_quote_field(Type, quote(Withdrawal))
    }},
    #{
        code => <<"unknown">>,
        reason => genlib:format(Details)
    };
build_failure(session, Withdrawal) ->
    Result = get_session_result(Withdrawal),
    {failed, Failure} = Result,
    Failure.

get_quote_field(provider_id, #{route := Route}) ->
    ff_withdrawal_routing:get_provider(Route);
get_quote_field(terminal_id, #{route := Route}) ->
    ff_withdrawal_routing:get_terminal(Route).

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
    Attempts = attempts(T),
    R = ff_withdrawal_route_attempt_utils:update_current_p_transfer(Tr, Attempts),
    update_attempts(R, T);
apply_event_({session_started, SessionID}, T) ->
    Session = #{id => SessionID},
    Attempts = attempts(T),
    R = ff_withdrawal_route_attempt_utils:update_current_session(Session, Attempts),
    update_attempts(R, T);
apply_event_({session_finished, {SessionID, Result}}, T) ->
    Attempts = attempts(T),
    Session = ff_withdrawal_route_attempt_utils:get_current_session(Attempts),
    SessionID = maps:get(id, Session),
    UpdSession = Session#{result => Result},
    R = ff_withdrawal_route_attempt_utils:update_current_session(UpdSession, Attempts),
    update_attempts(R, T);
apply_event_({route_changed, Route}, T) ->
    Attempts = attempts(T),
    R = ff_withdrawal_route_attempt_utils:new_route(Route, Attempts),
    T#{
        route => Route,
        attempts => R
    };
apply_event_({adjustment, _Ev} = Event, T) ->
    apply_adjustment_event(Event, T).

get_attempt_limit(Withdrawal) ->
    #{
        body := Body,
        params := #{
            wallet_id := WalletID,
            destination_id := DestinationID
        },
        created_at := Timestamp,
        party_revision := PartyRevision,
        domain_revision := DomainRevision,
        resource := Resource
    } = Withdrawal,
    {ok, Wallet} = get_wallet(WalletID),
    {ok, Destination} = get_destination(DestinationID),
    Identity = get_wallet_identity(Wallet),
    PartyID = ff_identity:party(Identity),
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
    #domain_TermSet{wallets = WalletTerms} = Terms,
    #domain_WalletServiceTerms{withdrawals = WithdrawalTerms} = WalletTerms,
    #domain_WithdrawalServiceTerms{attempt_limit = AttemptLimit} = WithdrawalTerms,
    get_attempt_limit_(AttemptLimit).

get_attempt_limit_(undefined) ->
    %% When attempt_limit is undefined
    %% do not try all defined providers, if any
    %% just stop after first one
    1;
get_attempt_limit_({value, Limit}) ->
    ff_dmsl_codec:unmarshal(attempt_limit, Limit).

%% Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> _.

-spec match_error_whitelist_test() -> _.
match_error_whitelist_test() ->
    ErrorWhitelist = binaries_to_error_tokens([
        <<"some:test:error">>,
        <<"another:test:error">>,
        <<"wide">>
    ]),
    ?assertEqual(false, match_error_whitelist(
        [<<>>],
        ErrorWhitelist
    )),
    ?assertEqual(false, match_error_whitelist(
        [<<"some">>, <<"completely">>, <<"different">>, <<"error">>],
        ErrorWhitelist
    )),
    ?assertEqual(false, match_error_whitelist(
        [<<"some">>, <<"test">>],
        ErrorWhitelist
    )),
    ?assertEqual(false, match_error_whitelist(
        [<<"wider">>],
        ErrorWhitelist
    )),
    ?assertEqual(true,  match_error_whitelist(
        [<<"some">>, <<"test">>, <<"error">>],
        ErrorWhitelist
    )),
    ?assertEqual(true,  match_error_whitelist(
        [<<"another">>, <<"test">>, <<"error">>, <<"that">>, <<"is">>, <<"more">>, <<"specific">>],
        ErrorWhitelist
    )).

-endif.
