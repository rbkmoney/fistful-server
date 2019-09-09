%%%
%%% Withdrawal
%%%

-module(ff_withdrawal).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_withdrawals_provider_adapter_thrift.hrl").

-type id() :: binary().

-define(ACTUAL_FORMAT_VERSION, 2).
-opaque withdrawal() :: #{
    version       := ?ACTUAL_FORMAT_VERSION,
    id            := id(),
    transfer_type := withdrawal,
    body          := body(),
    params        := transfer_params(),
    session       => session(),
    route         => route(),
    p_transfer    => p_transfer(),
    resource      => destination_resource(),
    limit_checks  => [limit_check_details()],
    status        => status(),
    external_id   => id()
}.
-type params() :: #{
    wallet_id            := ff_wallet_machine:id(),
    destination_id       := ff_destination:id(),
    body                 := body(),
    external_id          => id(),
    quote                => quote(),
    destination_resource => destination_resource()
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
    {terms, ff_party:validate_withdrawal_creation_error()} |
    {destination_resource, {bin_data, not_found}} |
    {contract, ff_party:get_contract_terms_error()}.

-type route() :: #{
    provider_id := provider_id()
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

-type gen_args() :: #{
    id            := id(),
    body          := body(),
    params        := params(),
    transfer_type := withdrawal,

    status        => status(),
    external_id   => external_id()
}.

-type limit_check_details() ::
    {wallet, wallet_limit_check_details()}.

-type wallet_limit_check_details() ::
    ok |
    {failed, wallet_limit_check_error()}.

-type wallet_limit_check_error() :: #{
    expected_range := cash_range(),
    balance := cash()
}.

-type action() :: poll | continue | undefined.

-export_type([withdrawal/0]).
-export_type([id/0]).
-export_type([params/0]).
-export_type([event/0]).
-export_type([route/0]).
-export_type([quote/0]).
-export_type([quote_params/0]).
-export_type([gen_args/0]).
-export_type([create_error/0]).
-export_type([action/0]).

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
-export([external_id/1]).
-export([destination_resource/1]).

%% API

-export([create/2]).
-export([gen/1]).
-export([get_quote/1]).
-export([is_finished/1]).

%% Event source

-export([apply_event/2]).
-export([maybe_migrate/1]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2, valid/2]).

%% Internal types

-type body()                  :: ff_transaction:body().
-type account()               :: ff_account:account().
-type wallet_id()             :: ff_wallet:id().
-type wallet()                :: ff_wallet:wallet().
-type cash_flow_plan()        :: ff_cash_flow:cash_flow_plan().
-type destination_id()        :: ff_destination:id().
-type process_result()        :: {action(), [event()]}.
-type final_cash_flow()       :: ff_cash_flow:final_cash_flow().
-type external_id()           :: id() | undefined.
-type p_transfer()            :: ff_postings_transfer:transfer().
-type session_id()            :: id().
-type destination_resource()  :: ff_destination:resource_full().
-type varset()                :: hg_selector:varset().
-type cash()                  :: ff_cash:cash().
-type cash_range()            :: ff_range:range(cash()).
-type failure()               :: ff_failure:failure().
-type session_result()        :: ff_withdrawal_session:session_result().

-type wrapped_adjustment_event()  :: ff_adjustment_utils:wrapped_event().

% TODO I'm now sure about this change, it may crash old events. Or not. ))
-type provider_id() :: pos_integer() | id().

-type legacy_event() :: any().

-type transfer_params() :: #{
    wallet_id      := wallet_id(),
    destination_id := destination_id(),
    quote          => quote()
}.

-type session() :: #{
    id     := session_id(),
    result => session_result()
}.

-type quote_validation_data() :: #{
    binary() => any()
}.

-type varset_params() :: #{
    body                 := body(),
    wallet               := ff_wallet:wallet(),
    destination          => ff_destination:destination(),
    destination_resource => destination_resource()
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
    stop |  % Legacy activity
    finish.

-type fail_type() ::
    limit_check |
    route_not_found |
    {inconsistent_quote_route, provider_id()} |
    session.

%% Accessors

-spec wallet_id(withdrawal()) -> wallet_id().
wallet_id(T) ->
    maps:get(wallet_id, params(T)).

-spec destination_id(withdrawal()) -> destination_id().
destination_id(T) ->
    maps:get(destination_id, params(T)).

-spec destination_resource(withdrawal()) ->
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

-spec quote(withdrawal()) -> quote() | undefined.
quote(T) ->
    maps:get(quote, T, undefined).

-spec id(withdrawal()) -> id().
id(#{id := V}) ->
    V.

-spec body(withdrawal()) -> body().
body(#{body := V}) ->
    V.

-spec status(withdrawal()) -> status() | undefined.
status(T) ->
    maps:get(status, T, undefined).

-spec route(withdrawal()) -> route() | undefined.
route(T) ->
    maps:get(route, T, undefined).

-spec external_id(withdrawal()) -> external_id() | undefined.
external_id(T) ->
    maps:get(external_id, T, undefined).

%% API

-spec gen(gen_args()) ->
    withdrawal().
gen(Args) ->
    TypeKeys = [id, transfer_type, body, params, status, external_id],
    genlib_map:compact(maps:with(TypeKeys, Args)).

-spec create(id(), params()) ->
    {ok, [event()]} |
    {error, create_error()}.
create(ID, Params) ->
    do(fun() ->
        #{wallet_id := WalletID, destination_id := DestinationID, body := Body} = Params,
        Wallet = ff_wallet_machine:wallet(unwrap(wallet, ff_wallet_machine:get(WalletID))),
        WalletAccount = ff_wallet:account(Wallet),
        Destination = ff_destination:get(
            unwrap(destination, ff_destination:get_machine(DestinationID))
        ),
        ok = unwrap(destination, valid(authorized, ff_destination:status(Destination))),

        Quote = maps:get(quote, Params, undefined),
        ResourceID = quote_resource_id(Quote),

        Resource = unwrap(destination_resource, ff_destination:resource_full(Destination, ResourceID)),
        {ok, IdentityMachine} = ff_identity_machine:get(ff_wallet:identity(Wallet)),
        Identity = ff_identity_machine:identity(IdentityMachine),
        PartyID = ff_identity:party(Identity),
        ContractID = ff_identity:contract(Identity),
        VS = collect_varset(make_varset_params(Body, Wallet, Destination, Resource)),
        Terms = unwrap(contract, ff_party:get_contract_terms(PartyID, ContractID, VS, ff_time:now())),
        valid = unwrap(terms, ff_party:validate_withdrawal_creation(Terms, Body, WalletAccount)),

        TransferParams = genlib_map:compact(#{
            wallet_id => WalletID,
            destination_id => DestinationID,
            quote => maps:get(quote, Params, undefined)
        }),
        ExternalID = maps:get(external_id, Params, undefined),
        [
            {created, add_external_id(ExternalID, #{
                version       => ?ACTUAL_FORMAT_VERSION,
                id            => ID,
                transfer_type => withdrawal,
                body          => Body,
                params        => TransferParams
            })},
            {status_changed, pending},
            {resource_got, Resource}
        ]
    end).

%% Сущность завершила свою основную задачу по переводу денег. Дальше её состояние будет меняться только
%% изменением дочерних сущностей, например запуском adjustment.
-spec is_finished(withdrawal()) -> boolean().
is_finished(#{status := succeeded}) ->
    true;
is_finished(#{status := {failed, _}}) ->
    true;
is_finished(#{status := pending}) ->
    false.

%% Transfer callbacks

-spec process_transfer(withdrawal()) ->
    process_result().
process_transfer(Withdrawal) ->
    Activity = deduce_activity(Withdrawal),
    do_process_transfer(Activity, Withdrawal).

%% Internal getters

-spec params(withdrawal()) -> transfer_params().
params(#{params := V}) ->
    V.

-spec p_transfer(withdrawal()) -> p_transfer() | undefined.
p_transfer(Withdrawal) ->
    maps:get(p_transfer, Withdrawal, undefined).

-spec p_transfer_status(withdrawal()) -> ff_postings_transfer:status() | undefined.
p_transfer_status(Withdrawal) ->
    case p_transfer(Withdrawal) of
        undefined ->
            undefined;
        #{status := Status} ->
            Status
    end.

-spec route_selection_status(withdrawal()) -> unknown | found.
route_selection_status(Withdrawal) ->
    case route(Withdrawal) of
        undefined ->
            unknown;
        _Known ->
            found
    end.

add_external_id(undefined, Event) ->
    Event;
add_external_id(ExternalID, Event) ->
    Event#{external_id => ExternalID}.

%% Processing helpers

-spec deduce_activity(withdrawal()) ->
    activity().
deduce_activity(Withdrawal) ->
    Params = #{
        route => route_selection_status(Withdrawal),
        p_transfer => p_transfer_status(Withdrawal),
        session => session_processing_status(Withdrawal),
        status => status(Withdrawal),
        limit_check => limit_check_processing_status(Withdrawal)
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

%% Legacy activity. Remove after first deployment
do_finished_activity(#{status := {failed, _}, p_transfer := prepared}) ->
    p_transfer_cancel;
do_finished_activity(#{status := succeeded, p_transfer := prepared}) ->
    p_transfer_commit;
do_finished_activity(#{status := succeeded, p_transfer := committed}) ->
    stop;
do_finished_activity(#{status := {failed, _}, p_transfer := cancelled}) ->
    stop.

-spec do_process_transfer(activity(), withdrawal()) ->
    process_result().
do_process_transfer(routing, Withdrawal) ->
    process_routing(Withdrawal);
do_process_transfer(p_transfer_start, Withdrawal) ->
    process_p_transfer_creation(Withdrawal);
do_process_transfer(p_transfer_prepare, Withdrawal) ->
    {ok, Events} = ff_pipeline:with(p_transfer, Withdrawal, fun ff_postings_transfer:prepare/1),
    {continue, Events};
do_process_transfer(p_transfer_commit, Withdrawal) ->
    {ok, Events} = ff_pipeline:with(p_transfer, Withdrawal, fun ff_postings_transfer:commit/1),
    {continue, Events};
do_process_transfer(p_transfer_cancel, Withdrawal) ->
    {ok, Events} = ff_pipeline:with(p_transfer, Withdrawal, fun ff_postings_transfer:cancel/1),
    {continue, Events};
do_process_transfer(limit_check, Withdrawal) ->
    process_limit_check(Withdrawal);
do_process_transfer(session_starting, Withdrawal) ->
    process_session_creation(Withdrawal);
do_process_transfer(session_polling, Withdrawal) ->
    process_session_poll(Withdrawal);
do_process_transfer({fail, Reason}, Withdrawal) ->
    process_transfer_fail(Reason, Withdrawal);
do_process_transfer(finish, Withdrawal) ->
    process_transfer_finish(Withdrawal);
do_process_transfer(stop, _Withdrawal) ->
    {undefined, []}.

-spec process_routing(withdrawal()) ->
    process_result().
process_routing(Withdrawal) ->
    case do_process_routing(Withdrawal) of
        {ok, ProviderID} ->
            {continue, [
                {route_changed, #{provider_id => ProviderID}}
            ]};
        {error, route_not_found} ->
            process_transfer_fail(route_not_found, Withdrawal);
        {error, {inconsistent_quote_route, _ProviderID} = Reason} ->
            process_transfer_fail(Reason, Withdrawal)
    end.

-spec do_process_routing(withdrawal()) -> {ok, provider_id()} | {error, Reason} when
    Reason :: route_not_found | {inconsistent_quote_route, provider_id()}.
do_process_routing(Withdrawal) ->
    Body = body(Withdrawal),
    {ok, WalletMachine} = ff_wallet_machine:get(wallet_id(Withdrawal)),
    Wallet = ff_wallet_machine:wallet(WalletMachine),
    {ok, DestinationMachine} = ff_destination:get_machine(destination_id(Withdrawal)),
    Destination = ff_destination:get(DestinationMachine),
    Resource = destination_resource(Withdrawal),
    do(fun() ->
        ProviderID = unwrap(prepare_route(Wallet, Body, Destination, Resource)),
        valid = unwrap(validate_quote_provider(ProviderID, quote(Withdrawal))),
        ProviderID
    end).

-spec prepare_route(
    wallet(),
    body(),
    ff_destination:destination() | undefined,
    destination_resource() | undefined
) ->
    {ok, provider_id()} | {error, route_not_found}.

prepare_route(Wallet, Body, Destination, Resource) ->
    {ok, PaymentInstitutionID} = ff_party:get_wallet_payment_institution_id(Wallet),
    {ok, PaymentInstitution} = ff_payment_institution:get(PaymentInstitutionID),
    VS = collect_varset(make_varset_params(Body, Wallet, Destination, Resource)),
    case ff_payment_institution:compute_withdrawal_providers(PaymentInstitution, VS) of
        {ok, Providers}  ->
            choose_provider(Providers, VS);
        {error, {misconfiguration, _Details} = Error} ->
            %% TODO: Do not interpret such error as an empty route list.
            %% The current implementation is made for compatibility reasons.
            %% Try to remove and follow the tests.
            _ = logger:warning("Route search failed: ~p", [Error]),
            {error, route_not_found}
    end.

-spec validate_quote_provider(provider_id(), quote()) ->
    {ok, valid} | {error, {inconsistent_quote_route, provider_id()}}.
validate_quote_provider(_ProviderID, undefined) ->
    {ok, valid};
validate_quote_provider(ProviderID, #{quote_data := #{<<"provider_id">> := ProviderID}}) ->
    {ok, valid};
validate_quote_provider(ProviderID, _) ->
    {error, {inconsistent_quote_route, ProviderID}}.

-spec choose_provider([provider_id()], varset()) ->
    {ok, provider_id()} | {error, route_not_found}.
choose_provider(Providers, VS) ->
    case lists:filter(fun(P) -> validate_withdrawals_terms(P, VS) end, Providers) of
        [ProviderID | _] ->
            {ok, ProviderID};
        [] ->
            {error, route_not_found}
    end.

-spec validate_withdrawals_terms(provider_id(), varset()) ->
    boolean().
validate_withdrawals_terms(ID, VS) ->
    Provider = unwrap(ff_payouts_provider:get(ID)),
    case ff_payouts_provider:validate_terms(Provider, VS) of
        {ok, valid} ->
            true;
        {error, _Error} ->
            false
    end.

-spec process_limit_check(withdrawal()) ->
    process_result().
process_limit_check(Withdrawal) ->
    Body = body(Withdrawal),
    {ok, WalletMachine} = ff_wallet_machine:get(wallet_id(Withdrawal)),
    Wallet = ff_wallet_machine:wallet(WalletMachine),
    Events = case validate_wallet_limits(Wallet, Body) of
        {ok, valid} ->
            [{limit_check, {wallet, ok}}];
        {error, {terms_violation, {wallet_limit, {cash_range, {Cash, Range}}}}} ->
            Details = #{
                expected_range => Range,
                balance => Cash
            },
            [{limit_check, {wallet, {failed, Details}}}]
    end,
    {continue, Events}.

-spec process_p_transfer_creation(withdrawal()) ->
    process_result().
process_p_transfer_creation(Withdrawal) ->
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = params(Withdrawal),
    Body = body(Withdrawal),
    {_Amount, CurrencyID} = Body,
    #{provider_id := ProviderID} = route(Withdrawal),
    {ok, Provider} = ff_payouts_provider:get(ProviderID),
    ProviderAccounts = ff_payouts_provider:accounts(Provider),
    ProviderAccount = maps:get(CurrencyID, ProviderAccounts, undefined),

    {ok, WalletMachine} = ff_wallet_machine:get(WalletID),
    Wallet = ff_wallet_machine:wallet(WalletMachine),
    WalletAccount = ff_wallet:account(Wallet),
    {ok, PaymentInstitutionID} = ff_party:get_wallet_payment_institution_id(Wallet),
    {ok, PaymentInstitution} = ff_payment_institution:get(PaymentInstitutionID),
    {ok, DestinationMachine} = ff_destination:get_machine(DestinationID),
    Destination = ff_destination:get(DestinationMachine),
    DestinationAccount = ff_destination:account(Destination),
    VS = collect_varset(make_varset_params(
        body(Withdrawal),
        Wallet,
        Destination,
        destination_resource(Withdrawal)
    )),

    {ok, SystemAccounts} = ff_payment_institution:compute_system_accounts(PaymentInstitution, VS),

    SystemAccount = maps:get(CurrencyID, SystemAccounts, #{}),
    SettlementAccount = maps:get(settlement, SystemAccount, undefined),
    SubagentAccount = maps:get(subagent, SystemAccount, undefined),

    ProviderFee = ff_payouts_provider:compute_fees(Provider, VS),

    {ok, IdentityMachine} = ff_identity_machine:get(ff_wallet:identity(Wallet)),
    Identity = ff_identity_machine:identity(IdentityMachine),
    PartyID = ff_identity:party(Identity),
    ContractID = ff_identity:contract(Identity),
    {ok, Terms} = ff_party:get_contract_terms(PartyID, ContractID, VS, ff_time:now()),
    {ok, WalletCashFlowPlan} = ff_party:get_withdrawal_cash_flow_plan(Terms),
    {ok, CashFlowPlan} = ff_cash_flow:add_fee(WalletCashFlowPlan, ProviderFee),
    {ok, FinalCashFlow} = finalize_cash_flow(
        CashFlowPlan,
        WalletAccount,
        DestinationAccount,
        SettlementAccount,
        SubagentAccount,
        ProviderAccount,
        body(Withdrawal)
    ),
    PTransferID = construct_p_transfer_id(id(Withdrawal)),
    {ok, PostingsTransferEvents} = ff_postings_transfer:create(PTransferID, FinalCashFlow),
    {continue, [{p_transfer, Ev} || Ev <- PostingsTransferEvents]}.

-spec process_session_creation(withdrawal()) ->
    process_result().
process_session_creation(Withdrawal) ->
    ID = construct_session_id(id(Withdrawal)),
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

construct_session_id(ID) ->
    ID.

-spec construct_p_transfer_id(id()) -> id().
construct_p_transfer_id(ID) ->
    <<"ff/withdrawal/", ID/binary>>.

create_session(ID, TransferData, SessionParams) ->
    case ff_withdrawal_session_machine:create(ID, TransferData, SessionParams) of
        ok ->
            ok;
        {error, exists} ->
            ok
    end.

-spec process_session_poll(withdrawal()) ->
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

-spec process_transfer_finish(withdrawal()) ->
    process_result().
process_transfer_finish(_Withdrawal) ->
    {undefined, [{status_changed, succeeded}]}.

-spec process_transfer_fail(fail_type(), withdrawal()) ->
    process_result().
process_transfer_fail(FailType, Withdrawal) ->
    Failure = build_failure(FailType, Withdrawal),
    {undefined, [{status_changed, {failed, Failure}}]}.

-spec finalize_cash_flow(cash_flow_plan(), account(), account(), account(), account(), account(), body()) ->
    {ok, final_cash_flow()} | {error, _Error}.
finalize_cash_flow(CashFlowPlan, WalletAccount, DestinationAccount,
                    SettlementAccount, SubagentAccount, ProviderAccount, Body) ->
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
    ff_cash_flow:finalize(CashFlowPlan, Accounts, Constants).

-spec make_varset_params(
    body(),
    ff_wallet:wallet(),
    ff_destination:destination() | undefined,
    destination_resource() | undefined
) ->
    varset_params().
make_varset_params(Body, Wallet, Destination, Resource) ->
    genlib_map:compact(#{
        body => Body,
        wallet => Wallet,
        destination => Destination,
        destination_resource => Resource
    }).

-spec collect_varset(varset_params()) ->
    varset().
collect_varset(#{body := Body, wallet := Wallet} = Params) ->
    {_, CurrencyID} = Body,
    Currency = #domain_CurrencyRef{symbolic_code = CurrencyID},
    IdentityID = ff_wallet:identity(Wallet),

    {ok, IdentityMachine} = ff_identity_machine:get(IdentityID),
    Identity = ff_identity_machine:identity(IdentityMachine),
    PartyID = ff_identity:party(Identity),
    Destination = maps:get(destination, Params, undefined),
    Resource = maps:get(destination_resource, Params, undefined),
    PaymentTool = case {Destination, Resource} of
        {undefined, _} ->
            undefined;
        %% TODO remove this when complete all old withdrawals
        {Destination, undefined} ->
            construct_payment_tool(ff_destination:resource(Destination));
        {_, Resource} ->
            construct_payment_tool(Resource)
    end,
    genlib_map:compact(#{
        currency => Currency,
        cost => ff_dmsl_codec:marshal(cash, Body),
        party_id => PartyID,
        wallet_id => ff_wallet:id(Wallet),
        payout_method => #domain_PayoutMethodRef{id = wallet_info},
        % TODO it's not fair, because it's PAYOUT not PAYMENT tool.
        payment_tool => PaymentTool
    }).

-spec construct_payment_tool(ff_destination:resource_full() | ff_destination:resource()) ->
    dmsl_domain_thrift:'PaymentTool'().
construct_payment_tool({bank_card, ResourceBankCard}) ->
    {bank_card, #domain_BankCard{
        token           = maps:get(token, ResourceBankCard),
        bin             = maps:get(bin, ResourceBankCard),
        masked_pan      = maps:get(masked_pan, ResourceBankCard),
        payment_system  = maps:get(payment_system, ResourceBankCard),
        issuer_country  = maps:get(iso_country_code, ResourceBankCard, undefined),
        bank_name       = maps:get(bank_name, ResourceBankCard, undefined)
    }};

construct_payment_tool({crypto_wallet, CryptoWallet}) ->
    {crypto_currency, #domain_CryptoWallet{
        id              = maps:get(id, CryptoWallet),
        crypto_currency = maps:get(currency, CryptoWallet),
        destination_tag = maps:get(tag, CryptoWallet, undefined)
    }}.

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
        DestinationMachine = unwrap(destination, ff_destination:get_machine(DestinationID)),
        Destination = ff_destination:get(DestinationMachine),
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
        WalletMachine = unwrap(wallet, ff_wallet_machine:get(WalletID)),
        Wallet = ff_wallet_machine:wallet(WalletMachine),
        ProviderID = unwrap(route, prepare_route(Wallet, Body, Destination, Resource)),
        {Adapter, AdapterOpts} = ff_withdrawal_session:get_adapter_with_opts(ProviderID),
        GetQuoteParams = #{
            external_id => maps:get(external_id, Params, undefined),
            currency_from => CurrencyFrom,
            currency_to => CurrencyTo,
            body => Body
        },
        {ok, Quote} = ff_adapter_withdrawal:get_quote(Adapter, GetQuoteParams, AdapterOpts),
        %% add provider id to quote_data
        wrap_quote(ff_destination:resource_full_id(Resource), ProviderID, Quote)
    end).

wrap_quote(ResourceID, ProviderID, Quote = #{quote_data := QuoteData}) ->
    Quote#{quote_data := genlib_map:compact(#{
        <<"version">> => 1,
        <<"quote_data">> => QuoteData,
        <<"provider_id">> => ProviderID,
        <<"resource_id">> => ResourceID
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

%% Session management

-spec session(withdrawal()) -> session() | undefined.
session(Withdrawal) ->
    maps:get(session, Withdrawal, undefined).

-spec session_id(withdrawal()) -> session_id() | undefined.
session_id(T) ->
    case session(T) of
        undefined ->
            undefined;
        #{id := SessionID} ->
            SessionID
    end.

-spec session_result(withdrawal()) -> session_result() | unknown | undefined.
session_result(Withdrawal) ->
    case session(Withdrawal) of
        undefined ->
            undefined;
        #{result := Result} ->
            Result;
        #{} ->
            unknown
    end.

-spec session_processing_status(withdrawal()) ->
    undefined | pending | succeeded | failed.
session_processing_status(Withdrawal) ->
    case session_result(Withdrawal) of
        undefined ->
            undefined;
        unknown ->
            pending;
        {success, _TrxInfo} ->
            succeeded;
        {failed, _Failure} ->
            failed
    end.

%% Limit helpers

-spec limit_checks(withdrawal()) ->
    [limit_check_details()].
limit_checks(Withdrawal) ->
    maps:get(limit_checks, Withdrawal, []).

-spec add_limit_check(limit_check_details(), withdrawal()) ->
    withdrawal().
add_limit_check(Check, Withdrawal) ->
    Checks = limit_checks(Withdrawal),
    Withdrawal#{limit_checks => [Check | Checks]}.

-spec limit_check_status(withdrawal()) ->
    ok | {failed, limit_check_details()} | unknown.
limit_check_status(#{limit_checks := Checks}) ->
    case lists:dropwhile(fun is_limit_check_ok/1, Checks) of
        [] ->
            ok;
        [H | _Tail] ->
            {failed, H}
    end;
limit_check_status(Withdrawal) when not is_map_key(limit_checks, Withdrawal) ->
    unknown.

-spec limit_check_processing_status(withdrawal()) ->
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
is_limit_check_ok({wallet, ok}) ->
    true;
is_limit_check_ok({wallet, {failed, _Details}}) ->
    false.

-spec validate_wallet_limits(wallet(), cash()) ->
    {ok, valid} |
    {error, {terms_violation, {wallet_limit, {cash_range, {cash(), cash_range()}}}}}.
validate_wallet_limits(Wallet, Body) ->
    case ff_party:validate_wallet_limits(Wallet, Body) of
        {ok, valid} = Result ->
            Result;
        {error, {terms_violation, {cash_range, {Cash, CashRange}}}} ->
            {error, {terms_violation, {wallet_limit, {cash_range, {Cash, CashRange}}}}};
        {error, {invalid_terms, _Details} = Reason} ->
            erlang:error(Reason)
    end.

%% Failure helpers

-spec build_failure(fail_type(), withdrawal()) -> failure().
build_failure(limit_check, Withdrawal) ->
    {failed, Details} = limit_check_status(Withdrawal),
    case Details of
        {wallet, _WalletLimitDetails} ->
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

-spec apply_event(event() | legacy_event(), ff_maybe:maybe(withdrawal())) ->
    withdrawal().
apply_event(Ev, T) ->
    apply_event_(maybe_migrate(Ev), T).

-spec apply_event_(event(), ff_maybe:maybe(withdrawal())) ->
    withdrawal().
apply_event_({created, T}, undefined) ->
    T;
apply_event_({status_changed, Status}, T) ->
    maps:put(status, Status, T);
apply_event_({resource_got, Resource}, T) ->
    maps:put(resource, Resource, T);
apply_event_({limit_check, Details}, T) ->
    add_limit_check(Details, T);
apply_event_({p_transfer, Ev}, T) ->
    T#{p_transfer => ff_postings_transfer:apply_event(Ev, p_transfer(T))};
apply_event_({session_started, SessionID}, T) ->
    Session = #{id => SessionID},
    maps:put(session, Session, T);
apply_event_({session_finished, {SessionID, Result}}, T) ->
    #{id := SessionID} = Session = session(T),
    maps:put(session, Session#{result => Result}, T);
apply_event_({route_changed, Route}, T) ->
    maps:put(route, Route, T).

-spec maybe_migrate(event() | legacy_event()) ->
    event().
% Actual events
maybe_migrate(Ev = {created, #{version := ?ACTUAL_FORMAT_VERSION}}) ->
    Ev;
maybe_migrate(Ev = {status_changed, {failed, #{code := _}}}) ->
    Ev;
maybe_migrate(Ev = {session_finished, {_SessionID, _Status}}) ->
    Ev;
maybe_migrate({p_transfer, PEvent}) ->
    {p_transfer, ff_postings_transfer:maybe_migrate(PEvent, withdrawal)};
% Old events
maybe_migrate({created, #{version := 1, handler := ff_withdrawal} = T}) ->
    #{
        version     := 1,
        id          := ID,
        handler     := ff_withdrawal,
        source      := SourceAccount,
        destination := DestinationAccount,
        body        := Body,
        params      := #{
            destination := DestinationID,
            source      := SourceID
        }
    } = T,
    maybe_migrate({created, #{
        version       => 2,
        id            => ID,
        transfer_type => withdrawal,
        body          => Body,
        params        => #{
            wallet_id             => SourceID,
            destination_id        => DestinationID,
            wallet_account        => SourceAccount,
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
    }});
maybe_migrate({created, T}) ->
    DestinationID = maps:get(destination, T),
    {ok, DestinationSt} = ff_destination:get_machine(DestinationID),
    DestinationAcc = ff_destination:account(ff_destination:get(DestinationSt)),
    SourceID = maps:get(source, T),
    {ok, SourceSt} = ff_wallet_machine:get(SourceID),
    SourceAcc = ff_wallet:account(ff_wallet_machine:wallet(SourceSt)),
    maybe_migrate({created, T#{
        version     => 1,
        handler     => ff_withdrawal,
        source      => SourceAcc,
        destination => DestinationAcc,
        params => #{
            destination => DestinationID,
            source      => SourceID
        }
    }});
maybe_migrate({transfer, PTransferEv}) ->
    maybe_migrate({p_transfer, PTransferEv});
maybe_migrate({status_changed, {failed, LegacyFailure}}) ->
    Failure = #{
        code => <<"unknown">>,
        reason => genlib:format(LegacyFailure)
    },
    maybe_migrate({status_changed, {failed, Failure}});
maybe_migrate({session_finished, SessionID}) ->
    {ok, SessionMachine} = ff_withdrawal_session_machine:get(SessionID),
    Session = ff_withdrawal_session_machine:session(SessionMachine),
    {finished, Result} = ff_withdrawal_session:status(Session),
    maybe_migrate({session_finished, {SessionID, Result}});
% Other events
maybe_migrate(Ev) ->
    Ev.
