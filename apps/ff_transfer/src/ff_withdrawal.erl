%%%
%%% Withdrawal
%%%

-module(ff_withdrawal).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-type withdrawal() :: ff_transfer:transfer(transfer_params()).
-type transfer_params() :: #{
    wallet_id := wallet_id(),
    destination_id := destination_id(),
    wallet_account := account(),
    destination_account := account(),
    wallet_cash_flow_plan := cash_flow_plan()
}.

-type machine() :: ff_transfer_machine:st(transfer_params()).
-type events()  :: ff_transfer_machine:events(ff_transfer:event(transfer_params(), route())).
-type event()   :: ff_transfer_machine:event(ff_transfer:event(transfer_params(), route())).
-type route()   :: ff_transfer:route(#{
    provider_id := id()
}).

-export_type([withdrawal/0]).
-export_type([machine/0]).
-export_type([transfer_params/0]).
-export_type([events/0]).
-export_type([event/0]).
-export_type([route/0]).

%% ff_transfer_machine behaviour
-behaviour(ff_transfer_machine).
-export([process_transfer/1]).
-export([process_failure/2]).

%% Accessors

-export([wallet_id/1]).
-export([destination_id/1]).
-export([id/1]).
-export([body/1]).
-export([status/1]).
-export([params/1]).
-export([route/1]).
-export([external_id/1]).

%% API
-export([create/3]).
-export([get/1]).
-export([get_machine/1]).
-export([events/2]).

%% Event source

-export([maybe_migrate/1]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2, valid/2]).

%% Internal types

-type id() :: ff_transfer_machine:id().
-type body() :: ff_transfer:body().
-type account() :: ff_account:account().
-type provider() :: ff_withdrawal_provider:provider().
-type wallet_id() :: ff_wallet:id().
-type cash_flow_plan() :: ff_cash_flow:cash_flow_plan().
-type destination_id() :: ff_destination:id().
-type process_result() :: {ff_transfer_machine:action(), [event()]}.
-type final_cash_flow() :: ff_cash_flow:final_cash_flow().

%% Accessors

-spec wallet_id(withdrawal())       -> wallet_id().
-spec destination_id(withdrawal())  -> destination_id().
-spec id(withdrawal())              -> ff_transfer:id().
-spec body(withdrawal())            -> body().
-spec status(withdrawal())          -> ff_transfer:status().
-spec params(withdrawal())          -> transfer_params().
-spec route(withdrawal())           -> route().

wallet_id(T)       -> maps:get(wallet_id, ff_transfer:params(T)).
destination_id(T)  -> maps:get(destination_id, ff_transfer:params(T)).
id(T)              -> ff_transfer:id(T).
body(T)            -> ff_transfer:body(T).
status(T)          -> ff_transfer:status(T).
params(T)          -> ff_transfer:params(T).
route(T)           -> ff_transfer:route(T).

-spec external_id(withdrawal()) ->
    id() | undefined.
external_id(T)     -> ff_transfer:external_id(T).

%%

-define(NS, 'ff/withdrawal_v2').

-type ctx()    :: ff_ctx:ctx().
-type params() :: #{
    wallet_id      := ff_wallet_machine:id(),
    destination_id := ff_destination:id(),
    body           := ff_transaction:body(),
    external_id    => id()
}.

-spec create(id(), params(), ctx()) ->
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
        Terms = unwrap(contract, ff_party:get_contract_terms(Wallet, Body, ff_time:now())),
        valid = unwrap(terms, ff_party:validate_withdrawal_creation(Terms, Body, WalletAccount)),
        CashFlowPlan = unwrap(cash_flow_plan, ff_party:get_withdrawal_cash_flow_plan(Terms)),

        Params = #{
            handler     => ?MODULE,
            body        => Body,
            params      => #{
                wallet_id => WalletID,
                destination_id => DestinationID,
                wallet_account => WalletAccount,
                destination_account => ff_destination:account(Destination),
                wallet_cash_flow_plan => CashFlowPlan
            },
            external_id => maps:get(external_id, Args, undefined)
        },
        unwrap(ff_transfer_machine:create(?NS, ID, Params, Ctx))
    end).

-spec get(machine()) ->
    withdrawal().

get(St) ->
    ff_transfer_machine:transfer(St).

-spec get_machine(id()) ->
    {ok, machine()}       |
    {error, notfound}.

get_machine(ID) ->
    ff_transfer_machine:get(?NS, ID).

-spec events(id(), machinery:range()) ->
    {ok, events()} |
    {error, notfound}.

events(ID, Range) ->
    ff_transfer_machine:events(?NS, ID, Range).

%% ff_transfer_machine behaviour

-spec process_transfer(withdrawal()) ->
    {ok, process_result()} |
    {error, _Reason}.

process_transfer(Withdrawal) ->
    Activity = deduce_activity(Withdrawal),
    do_process_transfer(Activity, Withdrawal).

-spec process_failure(any(), withdrawal()) ->
    {ok, process_result()} |
    {error, _Reason}.

process_failure(Reason, Withdrawal) ->
    ff_transfer:process_failure(Reason, Withdrawal).

%% Internals

-type activity() ::
    routing                  |
    p_transfer_start         |
    session_starting         |
    session_polling          |
    idle                     .

% TODO: Move activity to ff_transfer
-spec deduce_activity(withdrawal()) ->
    activity().
deduce_activity(Withdrawal) ->
    Params = #{
        route => ff_transfer:route(Withdrawal),
        p_transfer => ff_transfer:p_transfer(Withdrawal),
        session_id => ff_transfer:session_id(Withdrawal),
        status => status(Withdrawal)
    },
    do_deduce_activity(Params).

do_deduce_activity(#{route := undefined}) ->
    routing;
do_deduce_activity(#{p_transfer := undefined}) ->
    p_transfer_start;
do_deduce_activity(#{p_transfer := #{status := prepared}, session_id := undefined}) ->
    session_starting;
do_deduce_activity(#{session_id := SessionID, status := pending}) when SessionID =/= undefined ->
    session_polling;
do_deduce_activity(_Other) ->
    idle.

do_process_transfer(routing, Withdrawal) ->
    create_route(Withdrawal);
do_process_transfer(p_transfer_start, Withdrawal) ->
    create_p_transfer(Withdrawal);
do_process_transfer(session_starting, Withdrawal) ->
    create_session(Withdrawal);
do_process_transfer(session_polling, Withdrawal) ->
    poll_session_completion(Withdrawal);
do_process_transfer(idle, Withdrawal) ->
    ff_transfer:process_transfer(Withdrawal).

-spec create_route(withdrawal()) ->
    {ok, process_result()} |
    {error, _Reason}.
create_route(Withdrawal) ->
    #{
        destination_id := DestinationID
    } = params(Withdrawal),
    do(fun () ->
        DestinationMachine = unwrap(destination, ff_destination:get_machine(DestinationID)),
        Destination = ff_destination:get(DestinationMachine),
        ProviderID = unwrap(route, ff_withdrawal_provider:choose(Destination, body(Withdrawal))),
        {continue, [{route_changed, #{provider_id => ProviderID}}]}
    end).

-spec create_p_transfer(withdrawal()) ->
    {ok, process_result()} |
    {error, _Reason}.
create_p_transfer(Withdrawal) ->
    #{
        wallet_account := WalletAccount,
        destination_account := DestinationAccount,
        wallet_cash_flow_plan := WalletCashFlowPlan
    } = params(Withdrawal),
    {_Amount, CurrencyID} = body(Withdrawal),
    do(fun () ->
        Provider = unwrap(provider, get_route_provider(route(Withdrawal))),
        ProviderAccounts = ff_withdrawal_provider:accounts(Provider),
        ProviderAccount = maps:get(CurrencyID, ProviderAccounts, undefined),
        ProviderFee = ff_withdrawal_provider:fee(Provider, CurrencyID),
        SystemAccounts = unwrap(system, get_system_accounts()),
        SystemAccount = maps:get(CurrencyID, SystemAccounts, undefined),
        CashFlowPlan = unwrap(provider_fee, ff_cash_flow:add_fee(WalletCashFlowPlan, ProviderFee)),
        FinalCashFlow = unwrap(cash_flow, finalize_cash_flow(
            CashFlowPlan, WalletAccount, DestinationAccount, SystemAccount, ProviderAccount, body(Withdrawal)
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
        wallet_account := WalletAccount,
        destination_account := DestinationAccount
    } = params(Withdrawal),
    do(fun () ->
        valid = unwrap(ff_party:validate_wallet_limits(WalletID, Body, WalletAccount)),
        #{provider_id := ProviderID} = route(Withdrawal),
        SenderSt = unwrap(ff_identity_machine:get(ff_account:identity(WalletAccount))),
        ReceiverSt = unwrap(ff_identity_machine:get(ff_account:identity(DestinationAccount))),
        TransferData = #{
            id          => ID,
            cash        => body(Withdrawal),
            sender      => ff_identity_machine:identity(SenderSt),
            receiver    => ff_identity_machine:identity(ReceiverSt)
        },
        SessionParams = #{
            destination => destination_id(Withdrawal),
            provider_id => ProviderID
        },
        ok = unwrap(ff_withdrawal_session_machine:create(ID, TransferData, SessionParams)),
        {continue, [{session_started, ID}]}
    end).

construct_session_id(ID) ->
    ID.

-spec construct_p_transfer_id(id()) -> id().
construct_p_transfer_id(ID) ->
    <<"ff/withdrawal/", ID/binary>>.

poll_session_completion(Withdrawal) ->
    SessionID = ff_transfer:session_id(Withdrawal),
    {ok, Session} = ff_withdrawal_session_machine:get(SessionID),
    do(fun () ->
        case ff_withdrawal_session_machine:status(Session) of
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

-spec finalize_cash_flow(cash_flow_plan(), account(), account(), account(), account(), body()) ->
    {ok, final_cash_flow()} | {error, _Error}.
finalize_cash_flow(CashFlowPlan, WalletAccount, DestinationAccount, SystemAccount, ProviderAccount, Body) ->
    Constants = #{
        operation_amount => Body
    },
    Accounts = genlib_map:compact(#{
        {wallet, sender_settlement} => WalletAccount,
        {wallet, receiver_destination} => DestinationAccount,
        {system, settlement} => SystemAccount,
        {provider, settlement} => ProviderAccount
    }),
    ff_cash_flow:finalize(CashFlowPlan, Accounts, Constants).

-spec maybe_migrate(ff_transfer:event() | ff_transfer:legacy_event()) ->
    ff_transfer:event().
maybe_migrate(Ev) ->
    ff_transfer:maybe_migrate(Ev, withdrawal).
