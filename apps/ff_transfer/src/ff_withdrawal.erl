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
    session_id    => session_id(),
    route         => any(),
    p_transfer    => p_transfer() | undefined,
    resource      => destination_resource(),
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
    {failed, _TODO} .

-type event() ::
    {created, withdrawal()} |
    {route_changed, route()} |
    {p_transfer, ff_postings_transfer:event()} |
    {resource_got, destination_resource()} |
    {session_started, session_id()} |
    {session_finished, session_id()} |
    {status_changed, status()}.

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
-export([process_failure/2]).

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

%% Event source

-export([apply_event/2]).
-export([maybe_migrate/1]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2, valid/2]).

%% Internal types

-type body() :: ff_transaction:body().
-type account() :: ff_account:account().
-type provider() :: ff_withdrawal_provider:provider().
-type wallet_id() :: ff_wallet:id().
-type wallet() :: ff_wallet:wallet().
-type cash_flow_plan() :: ff_cash_flow:cash_flow_plan().
-type destination_id() :: ff_destination:id().
-type process_result() :: {action(), [event()]}.
-type final_cash_flow() :: ff_cash_flow:final_cash_flow().
-type external_id() :: id() | undefined.
-type p_transfer() :: ff_postings_transfer:transfer().
-type p_transfer_status() :: ff_postings_transfer:status().
-type session_id() :: id().
-type destination_resource() :: ff_destination:resource_full().

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

-type varset_params() :: #{
    body                 := body(),
    wallet               := ff_wallet:wallet(),
    destination          => ff_destination:destination(),
    destination_resource => destination_resource()
}.

%% Accessors

-spec wallet_id(withdrawal()) -> wallet_id().
wallet_id(T) ->
    maps:get(wallet_id, params(T)).

-spec destination_id(withdrawal()) -> destination_id().
destination_id(T) ->
    maps:get(destination_id, params(T)).

-spec destination_resource(withdrawal()) ->
    destination_resource().
destination_resource(T) ->
    case maps:find(resource, T) of
        {ok, Resource} ->
            Resource;
        error ->
            DestinationID = destination_id(T),
            {ok, DestinationMachine} = ff_destination:get_machine(DestinationID),
            Destination = ff_destination:get(DestinationMachine),
            {ok, Resource} = ff_destination:resource_full(Destination),
            Resource
    end.

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
        {ok, VS} = collect_varset(make_varset_params(Body, Wallet, Destination, Resource)),
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

%% Transfer callbacks

-spec process_transfer(withdrawal()) ->
    {ok, process_result()} |
    {error, _Reason}.

process_transfer(Withdrawal) ->
    Activity = deduce_activity(Withdrawal),
    do_process_transfer(Activity, Withdrawal).

-spec process_failure(any(), withdrawal()) ->
    {ok, process_result()} |
    {error, _Reason}.
process_failure(Reason, Deposit) ->
    {ok, ShutdownEvents} = do_process_failure(Reason, Deposit),
    {ok, {undefined, ShutdownEvents ++ [{status_changed, {failed, Reason}}]}}.

%% Internals

-type activity() ::
    routing |
    p_transfer_start |
    p_transfer_prepare |
    session_starting |
    session_polling |
    p_transfer_commit |
    p_transfer_cancel.

-spec params(withdrawal()) -> transfer_params().
params(#{params := V}) ->
    V.

-spec session_id(withdrawal()) -> session_id() | undefined.
session_id(T) ->
    maps:get(session_id, T, undefined).

add_external_id(undefined, Event) ->
    Event;
add_external_id(ExternalID, Event) ->
    Event#{external_id => ExternalID}.

-spec p_transfer(withdrawal()) -> p_transfer() | undefined.
p_transfer(T) ->
    maps:get(p_transfer, T, undefined).

-spec deduce_activity(withdrawal()) ->
    activity().
deduce_activity(Withdrawal) ->
    Params = #{
        route => route(Withdrawal),
        p_transfer => p_transfer(Withdrawal),
        session_id => session_id(Withdrawal),
        status => status(Withdrawal)
    },
    do_deduce_activity(Params).

do_deduce_activity(#{route := undefined}) ->
    routing;
do_deduce_activity(#{p_transfer := undefined}) ->
    p_transfer_start;
do_deduce_activity(#{status := pending, p_transfer := #{status := created}}) ->
    p_transfer_prepare;
do_deduce_activity(#{p_transfer := #{status := prepared}, session_id := undefined}) ->
    session_starting;
do_deduce_activity(#{session_id := SessionID, status := pending}) when SessionID =/= undefined ->
    session_polling;
do_deduce_activity(#{status := succeeded, p_transfer := #{status := prepared}}) ->
    p_transfer_commit;
do_deduce_activity(#{status := {failed, _}, p_transfer := #{status := prepared}}) ->
    p_transfer_cancel.

do_process_transfer(routing, Withdrawal) ->
    create_route(Withdrawal);
do_process_transfer(p_transfer_start, Withdrawal) ->
    create_p_transfer(Withdrawal);
do_process_transfer(p_transfer_prepare, Deposit) ->
    {ok, Events} = ff_pipeline:with(p_transfer, Deposit, fun ff_postings_transfer:prepare/1),
    {ok, {continue, Events}};
do_process_transfer(session_starting, Withdrawal) ->
    create_session(Withdrawal);
do_process_transfer(session_polling, Withdrawal) ->
    poll_session_completion(Withdrawal);
do_process_transfer(p_transfer_commit, Deposit) ->
    {ok, Events} = ff_pipeline:with(p_transfer, Deposit, fun ff_postings_transfer:commit/1),
    {ok, {undefined, Events}};
do_process_transfer(p_transfer_cancel, Deposit) ->
    {ok, Events} = ff_pipeline:with(p_transfer, Deposit, fun ff_postings_transfer:cancel/1),
    {ok, {undefined, Events}}.

do_process_failure(_Reason, #{status := pending, p_transfer := #{status := created}}) ->
    {ok, []};
do_process_failure(_Reason, #{status := pending, p_transfer := #{status := prepared}} = Deposit) ->
    ff_pipeline:with(p_transfer, Deposit, fun ff_postings_transfer:cancel/1);
do_process_failure(Reason, #{status := pending, p_transfer := #{status := committed}}) ->
    erlang:error({unprocessable_failure, committed_p_transfer, Reason});
do_process_failure(_Reason, Transfer) ->
    no_p_transfer = maps:get(p_transfer, Transfer, no_p_transfer),
    {ok, []}.

-spec create_route(withdrawal()) ->
    {ok, process_result()} |
    {error, _Reason}.
create_route(Withdrawal) ->
    #{
        wallet_id      := WalletID,
        destination_id := DestinationID
    } = params(Withdrawal),
    Body = body(Withdrawal),
    do(fun () ->
        {ok, WalletMachine} = ff_wallet_machine:get(WalletID),
        Wallet = ff_wallet_machine:wallet(WalletMachine),
        {ok, DestinationMachine} = ff_destination:get_machine(DestinationID),
        Destination = ff_destination:get(DestinationMachine),
        ProviderID = unwrap(prepare_route(
            Wallet,
            Body,
            Destination,
            destination_resource(Withdrawal)
        )),
        unwrap(validate_quote_provider(ProviderID, quote(Withdrawal))),
        {continue, [{route_changed, #{provider_id => ProviderID}}]}
    end).

-spec prepare_route(
    wallet(),
    body(),
    ff_destination:destination() | undefined,
    destination_resource() | undefined
) ->
    {ok, provider_id()} | {error, _Reason}.

prepare_route(Wallet, Body, Destination, Resource) ->
    do(fun () ->
        PaymentInstitutionID = unwrap(ff_party:get_wallet_payment_institution_id(Wallet)),
        PaymentInstitution = unwrap(ff_payment_institution:get(PaymentInstitutionID)),
        {ok, VS} = collect_varset(make_varset_params(Body, Wallet, Destination, Resource)),
        Providers = unwrap(ff_payment_institution:compute_withdrawal_providers(PaymentInstitution, VS)),
        unwrap(choose_provider(Providers, VS))
    end).

validate_quote_provider(_ProviderID, undefined) ->
    ok;
validate_quote_provider(ProviderID, #{quote_data := #{<<"provider_id">> := ProviderID}}) ->
    ok;
validate_quote_provider(_ProviderID, _) ->
    throw({quote, inconsistent_data}).

choose_provider(Providers, VS) ->
    case lists:filter(fun(P) -> validate_withdrawals_terms(P, VS) end, Providers) of
        [ProviderID | _] ->
            {ok, ProviderID};
        [] ->
            {error, route_not_found}
    end.

validate_withdrawals_terms(ID, VS) ->
    Provider = unwrap(ff_payouts_provider:get(ID)),
    case ff_payouts_provider:validate_terms(Provider, VS) of
        {ok, valid} ->
            true;
        {error, _Error} ->
            false
    end.

-spec create_p_transfer(withdrawal()) ->
    {ok, process_result()} |
    {error, _Reason}.
create_p_transfer(Withdrawal) ->
    #{provider_id := ProviderID} = route(Withdrawal),
    case is_integer(ProviderID) of
        true ->
            create_p_transfer_new_style(Withdrawal);
        false when is_binary(ProviderID) ->
            create_p_transfer_old_style(Withdrawal)
    end.

create_p_transfer_new_style(Withdrawal) ->
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = params(Withdrawal),
    Body = body(Withdrawal),
    {_Amount, CurrencyID} = Body,
    #{provider_id := ProviderID} = route(Withdrawal),
    do(fun () ->
        {ok, Provider} = ff_payouts_provider:get(ProviderID),
        ProviderAccounts = ff_payouts_provider:accounts(Provider),
        ProviderAccount = maps:get(CurrencyID, ProviderAccounts, undefined),

        {ok, WalletMachine} = ff_wallet_machine:get(WalletID),
        Wallet = ff_wallet_machine:wallet(WalletMachine),
        WalletAccount = ff_wallet:account(Wallet),
        PaymentInstitutionID = unwrap(ff_party:get_wallet_payment_institution_id(Wallet)),
        PaymentInstitution = unwrap(ff_payment_institution:get(PaymentInstitutionID)),
        {ok, DestinationMachine} = ff_destination:get_machine(DestinationID),
        Destination = ff_destination:get(DestinationMachine),
        DestinationAccount = ff_destination:account(Destination),
        VS = unwrap(collect_varset(make_varset_params(
            body(Withdrawal),
            Wallet,
            Destination,
            destination_resource(Withdrawal)
        ))),

        SystemAccounts = unwrap(ff_payment_institution:compute_system_accounts(PaymentInstitution, VS)),

        SystemAccount = maps:get(CurrencyID, SystemAccounts, #{}),
        SettlementAccount = maps:get(settlement, SystemAccount, undefined),
        SubagentAccount = maps:get(subagent, SystemAccount, undefined),

        ProviderFee = ff_payouts_provider:compute_fees(Provider, VS),

        {ok, IdentityMachine} = ff_identity_machine:get(ff_wallet:identity(Wallet)),
        Identity = ff_identity_machine:identity(IdentityMachine),
        PartyID = ff_identity:party(Identity),
        ContractID = ff_identity:contract(Identity),
        Terms = unwrap(contract, ff_party:get_contract_terms(PartyID, ContractID, VS, ff_time:now())),
        WalletCashFlowPlan = unwrap(cash_flow_plan, ff_party:get_withdrawal_cash_flow_plan(Terms)),
        CashFlowPlan = unwrap(provider_fee, ff_cash_flow:add_fee(WalletCashFlowPlan, ProviderFee)),
        FinalCashFlow = unwrap(cash_flow, finalize_cash_flow(
            CashFlowPlan,
            WalletAccount,
            DestinationAccount,
            SettlementAccount,
            SubagentAccount,
            ProviderAccount,
            body(Withdrawal)
        )),
        PTransferID = construct_p_transfer_id(id(Withdrawal)),
        PostingsTransferEvents = unwrap(p_transfer, ff_postings_transfer:create(PTransferID, FinalCashFlow)),
        {continue, [{p_transfer, Ev} || Ev <- PostingsTransferEvents]}
    end).

% TODO backward compatibility, remove after successful update
create_p_transfer_old_style(Withdrawal) ->
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = params(Withdrawal),
    Body = body(Withdrawal),
    {_Amount, CurrencyID} = Body,
    do(fun () ->
        {ok, Provider} = get_route_provider(route(Withdrawal)),
        ProviderAccounts = ff_withdrawal_provider:accounts(Provider),
        ProviderAccount = maps:get(CurrencyID, ProviderAccounts, undefined),
        ProviderFee = ff_withdrawal_provider:fee(Provider, CurrencyID),
        SystemAccounts = unwrap(system, get_system_accounts()),
        SettlementAccountMap = maps:get(settlement, SystemAccounts, #{}),
        SettlementAccount = maps:get(CurrencyID, SettlementAccountMap, undefined),
        SubagentAccountMap = maps:get(subagent, SystemAccounts, #{}),
        SubagentAccount = maps:get(CurrencyID, SubagentAccountMap, undefined),

        {ok, WalletMachine} = ff_wallet_machine:get(WalletID),
        Wallet = ff_wallet_machine:wallet(WalletMachine),
        WalletAccount = ff_wallet:account(Wallet),

        {ok, DestinationMachine} = ff_destination:get_machine(DestinationID),
        Destination = ff_destination:get(DestinationMachine),
        DestinationAccount = ff_destination:account(Destination),
        VS = unwrap(collect_varset(make_varset_params(
            body(Withdrawal),
            Wallet,
            Destination,
            destination_resource(Withdrawal)
        ))),

        {ok, IdentityMachine} = unwrap(ff_identity_machine:get(ff_wallet:identity(Wallet))),
        Identity = ff_identity_machine:identity(IdentityMachine),
        PartyID = ff_identity:party(Identity),
        ContractID = ff_identity:contract(Identity),

        Terms = unwrap(contract, ff_party:get_contract_terms(PartyID, ContractID, VS, ff_time:now())),
        WalletCashFlowPlan = unwrap(cash_flow_plan, ff_party:get_withdrawal_cash_flow_plan(Terms)),

        CashFlowPlan = unwrap(provider_fee, ff_cash_flow:add_fee(WalletCashFlowPlan, ProviderFee)),
        FinalCashFlow = unwrap(cash_flow, finalize_cash_flow(
            CashFlowPlan,
            WalletAccount,
            DestinationAccount,
            SettlementAccount,
            SubagentAccount,
            ProviderAccount,
            body(Withdrawal)
        )),
        PTransferID = construct_p_transfer_id(id(Withdrawal)),
        PostingsTransferEvents = unwrap(p_transfer, ff_postings_transfer:create(PTransferID, FinalCashFlow)),
        {continue, [{p_transfer, Ev} || Ev <- PostingsTransferEvents]}
    end).

-spec create_session(withdrawal()) ->
    {ok, process_result()} |
    {error, _Reason}.
create_session(Withdrawal) ->
    ID = construct_session_id(id(Withdrawal)),
    Body = body(Withdrawal),
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = params(Withdrawal),
    do(fun () ->
        {ok, WalletMachine} = ff_wallet_machine:get(WalletID),
        Wallet = ff_wallet_machine:wallet(WalletMachine),
        WalletAccount = ff_wallet:account(Wallet),

        {ok, DestinationMachine} = ff_destination:get_machine(DestinationID),
        Destination = ff_destination:get(DestinationMachine),
        DestinationAccount = ff_destination:account(Destination),

        valid = unwrap(ff_party:validate_wallet_limits(Wallet, Body)),
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
        {continue, [{session_started, ID}]}
    end).

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

poll_session_completion(Withdrawal) ->
    SessionID = session_id(Withdrawal),
    {ok, SessionMachine} = ff_withdrawal_session_machine:get(SessionID),
    Session = ff_withdrawal_session_machine:session(SessionMachine),
    do(fun () ->
        case ff_withdrawal_session:status(Session) of
            active ->
                {poll, []};
            {finished, {success, _}} ->
                {continue, [
                    {session_finished, SessionID},
                    {status_changed, succeeded}
                ]};
            {finished, {failed, Failure}} ->
                {continue, [
                    {session_finished, SessionID},
                    {status_changed, {failed, Failure}}
                ]}
        end
    end).

-spec get_route_provider(route()) -> {ok, provider()}.
get_route_provider(#{provider_id := ProviderID}) ->
    ff_withdrawal_provider:get(ProviderID).

-spec get_system_accounts() -> {ok, ff_withdrawal_provider:accounts()}.
get_system_accounts() ->
    % TODO: Read system account from domain config
    SystemConfig = maps:get(system, genlib_app:env(ff_transfer, withdrawal, #{})),
    SystemAccounts = maps:get(accounts, SystemConfig, undefined),
    {ok, SystemAccounts}.

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
    {ok, hg_selector:varset()} | no_return().

collect_varset(#{body := Body, wallet := Wallet} = Params) ->
    {_, CurrencyID} = Body,
    Currency = #domain_CurrencyRef{symbolic_code = CurrencyID},
    IdentityID = ff_wallet:identity(Wallet),

    do(fun() ->
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
        })
    end).

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

-spec get_quote(quote_params()) ->
    {ok, quote()} |
    {error,
        {destination, notfound}       |
        {destination, unauthorized}   |
        {route, _Reason}              |
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

get_quote_(Params = #{
    wallet_id := WalletID,
    body := Body,
    currency_from := CurrencyFrom,
    currency_to := CurrencyTo
}, Destination, Resource) ->
    do(fun() ->
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
%%

-spec apply_event(event() | legacy_event(), ff_maybe:maybe(withdrawal())) ->
    withdrawal().
apply_event(Ev, T) ->
    apply_event_(maybe_migrate(Ev), T).

-spec apply_event_(event(), ff_maybe:maybe(withdrawal())) ->
    withdrawal().
apply_event_({created, T}, undefined) ->
    T;
apply_event_({status_changed, S}, T) ->
    maps:put(status, S, T);
apply_event_({resource_got, R}, T) ->
    maps:put(resource, R, T);
apply_event_({p_transfer, Ev}, T = #{p_transfer := PT}) ->
    T#{p_transfer := ff_postings_transfer:apply_event(Ev, PT)};
apply_event_({p_transfer, Ev}, T) ->
    apply_event({p_transfer, Ev}, T#{p_transfer => undefined});
apply_event_({session_started, S}, T) ->
    maps:put(session_id, S, T);
apply_event_({session_finished, S}, T = #{session_id := S}) ->
    T;
apply_event_({route_changed, R}, T) ->
    maps:put(route, R, T).

-spec maybe_migrate(event() | legacy_event()) ->
    event().
% Actual events
maybe_migrate(Ev = {created, #{version := ?ACTUAL_FORMAT_VERSION}}) ->
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
% Other events
maybe_migrate(Ev) ->
    Ev.
