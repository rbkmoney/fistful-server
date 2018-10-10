%%%
%%% Withdrawal
%%%

-module(ff_withdrawal).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-type withdrawal() :: ff_transfer:transfer(transfer_params()).
-type transfer_params() :: #{
    source      := wallet_id(),
    destination := destination_id(),
    body        := body()
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

%% Accessors

-export([wallet_id/1]).
-export([destination_id/1]).
-export([id/1]).
-export([body/1]).
-export([status/1]).
-export([params/1]).

%% API
-export([create/3]).
-export([get/1]).
-export([get_machine/1]).
-export([events/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2, valid/2]).

%% Internal types

-type id() :: ff_transfer_machine:id().
-type body() :: ff_transfer:body().
-type wallet() :: ff_wallet:wallet().
-type account() :: ff_account:account().
-type final_cash_flow() :: ff_cash_flow:final_cash_flow().
-type cash_flow_plan() :: ff_cash_flow:cash_flow_plan().
-type wallet_id() :: ff_wallet:id().
-type timestamp() :: ff_time:timestamp_ms().
-type destination_id() :: ff_destination:id().
-type withdrawal_terms() :: dmsl_domain_thrift:'WithdrawalServiceTerms'().

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

%%

-define(NS, 'ff/withdrawal_v2').

-type ctx()    :: ff_ctx:ctx().
-type params() :: #{
    source      := ff_wallet_machine:id(),
    destination := ff_destination:id(),
    body        := ff_transaction:body()
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

create(ID, #{wallet_id := WalletID, destination_id := DestinationID, body := Body}, Ctx) ->
    do(fun() ->
        Wallet = ff_wallet_machine:wallet(unwrap(wallet, ff_wallet_machine:get(WalletID))),
        Destination = ff_destination:get(
            unwrap(destination, ff_destination:get_machine(DestinationID))
        ),
        ok = unwrap(destination, valid(authorized, ff_destination:status(Destination))),
        Terms = unwrap(withdrawal_terms, get_withdrawal_terms(Wallet, Body, ff_time:now())),
        CashFlowPlan = unwrap(cash_flow_plan, construct_cash_flow_plan(Terms)),
        Params = #{
            handler     => ?MODULE,
            body        => Body,
            params      => #{
                wallet_id => WalletID,
                destination_id => DestinationID,
                wallet_account => ff_wallet:account(Wallet),
                destination_account => ff_destination:account(Destination),
                wallet_cash_flow_plan => CashFlowPlan
            }
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
    {ok, {ff_transfer_machine:action(), [event()]}} |
    {error, _Reason}.

process_transfer(Transfer) ->
    Activity = deduce_activity(Transfer),
    do_process_transfer(Activity, Transfer).

%% Internals

-type activity() ::
    routing                  |
    p_transfer_start         |
    session_starting         |
    session_polling          |
    all_done                 .

% TODO: Move activity to ff_transfer
-spec deduce_activity(withdrawal()) ->
    activity().
deduce_activity(Transfer) ->
    Params = #{
        route => ff_transfer:route(Transfer),
        p_transfer => ff_transfer:p_transfer(Transfer),
        session_id => ff_transfer:session_id(Transfer),
        status => status(Transfer)
    },
    do_deduce_activity(Params).

do_deduce_activity(#{route := undefined}) ->
    routing;
do_deduce_activity(#{p_transfer := undefined}) ->
    p_transfer_start;
do_deduce_activity(#{session_id := undefined}) ->
    session_starting;
do_deduce_activity(#{status := pending}) ->
    session_polling;
do_deduce_activity(_Other) ->
    all_done.

do_process_transfer(routing, Transfer) ->
    create_route(Transfer);
do_process_transfer(p_transfer_start, Transfer) ->
    create_p_transfer(Transfer);
do_process_transfer(session_starting, Transfer) ->
    create_session(Transfer);
do_process_transfer(session_polling, Transfer) ->
    poll_session_completion(Transfer);
do_process_transfer(all_done, Transfer) ->
    ff_transfer:process_transfer(Transfer).

-spec create_route(withdrawal()) -> [event()].
create_route(Withdrawal) ->
    #{
        destination_id := DestinationID
    } = params(Withdrawal),
    {ok, DestinationMachine} = ff_destination:get_machine(DestinationID),
    Destination = ff_destination:get(DestinationMachine),
    {ok, ProviderID} = ff_withdrawal_provider:choose(Destination, body(Withdrawal)),
    [{route_changed, #{provider_id => ProviderID}}].

-spec create_p_transfer(withdrawal()) -> [event()].
create_p_transfer(Withdrawal) ->
    #{
        wallet_account := WalletAccount,
        destination_account := DestinationAccount,
        wallet_cash_flow_plan := CashFlowPlan
    } = params(Withdrawal),
    {ok, SystemAccount, ProviderAccount} = get_route_accounts(route(Withdrawal)),
    {ok, FinalCashFlow} = finalize_cash_flow(
        CashFlowPlan, WalletAccount, DestinationAccount, SystemAccount, ProviderAccount, body(Withdrawal)
    ),
    PTransferID = construct_p_transfer_id(id(Withdrawal)),
    PostingsTransferEvents = unwrap(ff_postings_transfer:create(PTransferID, FinalCashFlow)),
    [{p_transfer, Ev} || Ev <- PostingsTransferEvents].

-spec create_session(withdrawal()) -> [event()].
create_session(Withdrawal) ->
    ID = construct_session_id(id(Withdrawal)),
    #{
        wallet_account := WalletAccount,
        destination_account := DestinationAccount
    } = params(Withdrawal),
    {ok, SenderSt} = ff_identity_machine:get(ff_account:identity(WalletAccount)),
    {ok, ReceiverSt} = ff_identity_machine:get(ff_account:identity(DestinationAccount)),
    TransferData = #{
        id          => ID,
        cash        => body(Withdrawal),
        sender      => ff_identity_machine:identity(SenderSt),
        receiver    => ff_identity_machine:identity(ReceiverSt)
    },
    do(fun () ->
        ok = unwrap(ff_withdrawal_session_machine:create(ID, TransferData, #{destination => destination_id(Withdrawal)})),
        {continue, [{session_started, ID}]}
    end).

construct_session_id(ID) ->
    ID.

-spec construct_p_transfer_id(id()) -> id().
construct_p_transfer_id(ID) ->
    <<"ff/withdrawal/", ID/binary>>.

poll_session_completion(#{session := SID}) ->
    {ok, Session} = ff_withdrawal_session_machine:get(SID),
    do(fun () ->
        case ff_withdrawal_session_machine:status(Session) of
            active ->
                {poll, []};
            {finished, {success, _}} ->
                {continue, [
                    {session_finished, SID},
                    {status_changed, succeeded}
                ]};
            {finished, {failed, Failure}} ->
                {continue, [
                    {session_finished, SID},
                    {status_changed, {failed, Failure}}
                ]}
        end
    end).

-spec get_withdrawal_terms(wallet(), body(), timestamp()) -> Result when
    Result :: {ok, withdrawal_terms()} | {error, Error},
    Error ::
        {invalid_terms, _Details} |
        {party_not_found, id()} |
        {party_not_exists_yet, id()} |
        {exception, any()}.
get_withdrawal_terms(Wallet, Body, Timestamp) ->
    WalletID = ff_wallet:id(Wallet),
    Identity = ff_wallet:identity(Wallet),
    ContractID = ff_identity:contract(Identity),
    PartyID = ff_identity:party(Identity),
    {_Amount, CurrencyID} = Body,
    ff_party:get_withdrawal_terms(PartyID, ContractID, WalletID, CurrencyID, Timestamp).

-spec get_route_accounts(route()) -> {ok, System :: account(), Provider :: account()}.
get_route_accounts(#{provider_id := ProviderID}) ->
    ProviderAccount = ff_withdrawal_provider:get_account(ProviderID),
    % TODO: Read system account from domain config
    SystemAccount = genlib_map:get(account, genlib_map:get(system, genlib_app:env(ff_transfer, withdrawal, #{}))),
    {ok, ProviderAccount, SystemAccount}.

-spec construct_cash_flow_plan(withdrawal_terms()) ->
    {ok, cash_flow_plan()} | {error, _Error}.
construct_cash_flow_plan(Terms) ->
    #domain_WithdrawalServiceTerms{
        cash_flow = {value, EncodedCashFlowPlan}
    } = Terms,
    ff_cash_flow:decode_plan(EncodedCashFlowPlan).

-spec finalize_cash_flow(cash_flow_plan(), account(), account(), account(), account(), body()) ->
    {ok, final_cash_flow()} | {error, _Error}.
finalize_cash_flow(CashFlowPlan, WalletAccount, DestinationAccount, SystemAccount, ProviderAccount, Body) ->
    Constants = #{
        operation_amount => Body
    },
    Accounts = #{
        {wallet, sender_settlement} => WalletAccount,
        {wallet, receiver_destination} => DestinationAccount,
        {system, settlement} => SystemAccount,
        {provider, settlement} => ProviderAccount
    },
    ff_cash_flow:finalize(CashFlowPlan, Accounts, Constants).
