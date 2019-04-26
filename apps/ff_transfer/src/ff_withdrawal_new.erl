%%%
%%% Withdrawal
%%%

-module(ff_withdrawal_new).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-type withdrawal() :: ff_transfer_new:transfer(transfer_params()).
-type transfer_params() :: #{
    wallet_id             := wallet_id(),
    destination_id        := destination_id()
}.

-type machine() :: ff_transfer_machine_new:st(transfer_params()).
-type events()  :: ff_transfer_machine_new:events(ff_transfer_new:event(transfer_params(), route())).
-type event()   :: ff_transfer_machine_new:event(ff_transfer_new:event(transfer_params(), route())).
-type route()   :: ff_transfer_new:route(#{
    % TODO I'm now sure about this change, it may crash old events. Or not. ))
    provider_id := pos_integer() | id()
}).

-export_type([withdrawal/0]).
-export_type([machine/0]).
-export_type([transfer_params/0]).
-export_type([events/0]).
-export_type([event/0]).
-export_type([route/0]).

%% ff_transfer_new behaviour

-behaviour(ff_transfer_new).
-export([apply_event/2]).
-export([preprocess_transfer/1]).
-export([process_transfer/1]).
-export([process_failure/2]).
-export([process_call/2]).
-export([get_ns/0]).

%% Accessors

-export([wallet_id/1]).
-export([destination_id/1]).
-export([id/1]).
-export([body/1]).
-export([status/1]).
-export([params/1]).
-export([route/1]).
-export([external_id/1]).

%%
-export([transfer_type/0]).

%% API
-export([create/3]).
-export([get/1]).
-export([ctx/1]).
-export([get_machine/1]).
-export([events/2]).
-export([gen/1]).


%% Event source

-export([maybe_migrate/1]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2, valid/2]).

%% Internal types

-type id() :: ff_transfer_machine_new:id().
-type body() :: ff_transfer_new:body().
-type account() :: ff_account:account().
-type wallet_id() :: ff_wallet:id().
-type cash_flow_plan() :: ff_cash_flow:cash_flow_plan().
-type destination_id() :: ff_destination:id().
-type process_result() :: {ff_transfer_machine_new:action(), [event()]}.
-type final_cash_flow() :: ff_cash_flow:final_cash_flow().
-type session_params() :: #{
    cash        := ff_transaction:body(),
    sender      := ff_identity:identity(),
    receiver    := ff_identity:identity(),
    destination := ff_destination:id(),
    provider_id := ff_withdrawal_provider:id()
}.
-type ns()             :: machinery:namespace().

-spec transfer_type() ->
    ff_transfer_new:transfer_type().

transfer_type() ->
    ff_transfer_new:handler_to_type(?MODULE).

%% Constructor

-spec gen(ff_transfer_new:args()) ->
    withdrawal().

gen(Args) ->
    ff_transfer_new:gen(Args).

%% Accessors

-spec wallet_id(withdrawal())       -> wallet_id().
-spec destination_id(withdrawal())  -> destination_id().
-spec id(withdrawal())              -> ff_transfer_new:id().
-spec body(withdrawal())            -> body().
-spec status(withdrawal())          -> ff_transfer_new:status().
-spec params(withdrawal())          -> transfer_params().
-spec route(withdrawal())           -> route().

wallet_id(T)       -> maps:get(wallet_id, ff_transfer_new:params(T)).
destination_id(T)  -> maps:get(destination_id, ff_transfer_new:params(T)).
id(T)              -> ff_transfer_new:id(T).
body(T)            -> ff_transfer_new:body(T).
status(T)          -> ff_transfer_new:status(T).
params(T)          -> ff_transfer_new:params(T).
route(T)           -> ff_transfer_new:route(T).

-spec external_id(withdrawal()) ->
    id() | undefined.
external_id(T)     -> ff_transfer_new:external_id(T).

-define(NS, 'ff/withdrawal_v3').

%% API

-type ctx()    :: ff_ctx:ctx().
-type create_params() :: #{
    wallet_id      := ff_wallet_machine:id(),
    destination_id := ff_destination:id(),
    body           := ff_transaction:body(),
    external_id    => id()
}.

-spec create(id(), create_params(), ctx()) ->
    ok |
    {error,
        {source, notfound} |
        {destination, notfound | unauthorized} |
        {provider, notfound} |
        exists |
        _TransferError

    }.

create(ID, Args = #{wallet_id := WalletID, destination_id := DestinationID, body := Body}, Ctx) ->
    do(fun() ->
        Wallet = ff_wallet_machine:wallet(unwrap(wallet, ff_wallet_machine:get(WalletID))),
        WalletAccount = ff_wallet:account(Wallet),
        Destination = ff_destination:get(
            unwrap(destination, ff_destination:get_machine(DestinationID))
        ),
        ok = unwrap(destination, valid(authorized, ff_destination:status(Destination))),
        IdentityMachine = unwrap(ff_identity_machine:get(ff_wallet:identity(Wallet))),
        Identity = ff_identity_machine:identity(IdentityMachine),
        PartyID = ff_identity:party(Identity),
        ContractID = ff_identity:contract(Identity),
        VS = unwrap(collect_varset(Body, Wallet, Destination)),
        Terms = unwrap(contract, ff_party:get_contract_terms(PartyID, ContractID, VS, ff_time:now())),
        valid = unwrap(terms, ff_party:validate_withdrawal_creation(Terms, Body, WalletAccount)),

        Params = #{
            transfer_type   => ff_transfer_new:handler_to_type(?MODULE),
            id              => ID,
            body            => Body,
            params          => #{
                wallet_id       => WalletID,
                destination_id  => DestinationID
            },
            external_id     => maps:get(external_id, Args, undefined)
        },
        unwrap(ff_transfer_new:create(?NS, Params, Ctx))
    end).

-spec get(machine()) ->
    withdrawal().

get(St) ->
    ff_transfer_machine_new:transfer(St).

-spec ctx(machine()) ->
    ctx().

ctx(St) ->
    ff_transfer_machine_new:ctx(St).

-spec get_machine(id()) ->
    {ok, machine()}       |
    {error, notfound}.

get_machine(ID) ->
    ff_transfer_machine_new:get(?NS, ID).

-spec events(id(), machinery:range()) ->
    {ok, events()} |
    {error, notfound}.

events(ID, Range) ->
    ff_transfer_machine_new:events(?NS, ID, Range).

%% ff_transfer_machine_new behaviour

-spec get_ns() ->
    ns().

get_ns() ->
    ?NS.

-spec preprocess_transfer(withdrawal()) ->
    ok                                                                          |
    {ok, ff_transfer_new:new_activity(), ff_transfer_new:preprocess_result(session_params())}   |
    {error, _Reason}.

preprocess_transfer(undefined) ->
    {error, cant_preprocess_undefined_transfer};
preprocess_transfer(Withdrawal) ->
    Activity = ff_transfer_new:activity(Withdrawal),
    do_preprocess_transfer(Activity, Withdrawal).

-spec process_transfer(withdrawal()) ->
    {ok, process_result()} |
    {error, _Reason}.

process_transfer(undefined) ->
    {error, cant_process_undefined_transfer};
process_transfer(Withdrawal) ->
    Activity = ff_transfer_new:activity(Withdrawal),
    do_process_transfer(Activity, Withdrawal).

-spec process_call(_Args, withdrawal()) ->
    {ok, process_result()} |
    {error, _Reason}.

process_call({revert, _Body, _Reason}, _Withdrawal) ->
    {ok, {undefined, []}}.

-spec process_failure(any(), withdrawal()) ->
    {ok, process_result()} |
    {error, _Reason}.

process_failure(_Reason, _Withdrawal) ->
    {ok, {undefined, []}}.

%% Internals

do_preprocess_transfer(transaction_polling, Deposit) ->
    Transaction = ff_transfer_new:transaction(Deposit),
    case ff_transaction_new:activity(Transaction) of
        session_starting ->
            validate_wallet_limits(Deposit);
        _ ->
            ok
    end;
do_preprocess_transfer(transaction_starting, Withdrawal) ->
    {ok, transaction_starting, {create_transaction, create_transaction_params(Withdrawal)}};
do_preprocess_transfer(_, _) ->
    ok.

do_process_transfer(routing, Withdrawal) ->
    create_route(Withdrawal);
do_process_transfer(_, _) ->
    {ok, {undefined, []}}.

create_transaction_params(Withdrawal) ->
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = params(Withdrawal),

    %% Make final cashflow

    Body = body(Withdrawal),
    {_Amount, CurrencyID} = Body,
    #{provider_id := ProviderID} = route(Withdrawal),

    Provider = unwrap(provider, ff_payouts_provider:get(ProviderID)),
    ProviderAccounts = ff_payouts_provider:accounts(Provider),
    ProviderAccount = maps:get(CurrencyID, ProviderAccounts, undefined),

    Wallet = ff_wallet_machine:wallet(unwrap(wallet, ff_wallet_machine:get(WalletID))),
    WalletAccount = ff_wallet:account(Wallet),
    PaymentInstitutionID = unwrap(ff_party:get_wallet_payment_institution_id(Wallet)),
    PaymentInstitution = unwrap(ff_payment_institution:get(PaymentInstitutionID)),
    DestinationMachine = unwrap(destination, ff_destination:get_machine(DestinationID)),
    Destination = ff_destination:get(DestinationMachine),
    DestinationAccount = ff_destination:account(Destination),
    VS = unwrap(collect_varset(body(Withdrawal), Wallet, Destination)),
    SystemAccounts = unwrap(ff_payment_institution:compute_system_accounts(PaymentInstitution, VS)),

    SystemAccount = maps:get(CurrencyID, SystemAccounts, #{}),
    SettlementAccount = maps:get(settlement, SystemAccount, undefined),
    SubagentAccount = maps:get(subagent, SystemAccount, undefined),

    ProviderFee = ff_payouts_provider:compute_fees(Provider, VS),

    IdentityMachine = unwrap(ff_identity_machine:get(ff_wallet:identity(Wallet))),
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

    %% Prepare session params

    ID = construct_transaction_id(id(Withdrawal)),
    SenderSt = unwrap(ff_identity_machine:get(ff_account:identity(WalletAccount))),
    ReceiverSt = unwrap(ff_identity_machine:get(ff_account:identity(DestinationAccount))),
    SessionParams = #{
        cash        => body(Withdrawal),
        sender      => ff_identity_machine:identity(SenderSt),
        receiver    => ff_identity_machine:identity(ReceiverSt),
        destination => destination_id(Withdrawal),
        provider_id => ProviderID
    },

    #{
        id                  => ID,
        body                => body(Withdrawal),
        final_cash_flow     => FinalCashFlow,
        session_data        => #{
            type    => ff_transaction_new:get_session_type(withdrawal),
            params  => SessionParams
        }
    }.

-spec construct_transaction_id(id()) -> id().
construct_transaction_id(ID) ->
    <<"ff/withdrawal/", ID/binary>>.

validate_wallet_limits(Withdrawal) ->
    do(fun () ->
        #{
            wallet_id := WalletID
        } = params(Withdrawal),
        Body = body(Withdrawal),
        Wallet = ff_wallet_machine:wallet(unwrap(wallet, ff_wallet_machine:get(WalletID))),
        WalletAccount = ff_wallet:account(Wallet),
        valid = unwrap(ff_party:validate_wallet_limits(WalletID, Body, WalletAccount)),
        ok
    end).

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
        Wallet = ff_wallet_machine:wallet(unwrap(wallet, ff_wallet_machine:get(WalletID))),
        PaymentInstitutionID = unwrap(ff_party:get_wallet_payment_institution_id(Wallet)),
        PaymentInstitution = unwrap(ff_payment_institution:get(PaymentInstitutionID)),
        DestinationMachine = unwrap(destination, ff_destination:get_machine(DestinationID)),
        Destination = ff_destination:get(DestinationMachine),
        VS = unwrap(collect_varset(Body, Wallet, Destination)),
        Providers = unwrap(ff_payment_institution:compute_withdrawal_providers(PaymentInstitution, VS)),
        ProviderID = unwrap(choose_provider(Providers, VS)),
        {continue, [{route_changed, #{provider_id => ProviderID}}]}
    end).

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

-spec apply_event(event(), withdrawal()) ->
    withdrawal().
apply_event(_, T) ->
    T.

-spec maybe_migrate(ff_transfer_new:event() | ff_transfer_new:legacy_event()) ->
    ff_transfer_new:event().
maybe_migrate(Ev) ->
    ff_transfer_new:maybe_migrate(Ev, withdrawal).

-spec collect_varset(body(), ff_wallet:wallet(), ff_destination:destination()) ->
    {ok, hg_selector:varset()} | {error, Reason :: any()}.

collect_varset({_, CurrencyID} = Body, Wallet, Destination) ->
    Currency = #domain_CurrencyRef{symbolic_code = CurrencyID},
    IdentityID = ff_wallet:identity(Wallet),
    do(fun() ->
        IdentityMachine = unwrap(ff_identity_machine:get(IdentityID)),
        Identity = ff_identity_machine:identity(IdentityMachine),
        PartyID = ff_identity:party(Identity),
        PaymentTool = construct_payment_tool(ff_destination:resource(Destination)),
        #{
            currency => Currency,
            cost => ff_cash:encode(Body),
            % TODO it's not fair, because it's PAYOUT not PAYMENT tool.
            payment_tool => PaymentTool,
            party_id => PartyID,
            wallet_id => ff_wallet:id(Wallet),
            payout_method => #domain_PayoutMethodRef{id = wallet_info}
        }
    end).

-spec construct_payment_tool(ff_destination:resource()) ->
    dmsl_domain_thrift:'PaymentTool'().
construct_payment_tool({bank_card, ResourceBankCard}) ->
    {bank_card, #domain_BankCard{
        token           = maps:get(token, ResourceBankCard),
        payment_system  = maps:get(payment_system, ResourceBankCard),
        bin             = maps:get(bin, ResourceBankCard),
        masked_pan      = maps:get(masked_pan, ResourceBankCard)
    }}.
