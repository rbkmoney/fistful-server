%%%
%%% Transaction between 2 accounts
%%%

-module(ff_transaction_new).
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-define(ACTUAL_FORMAT_VERSION, 1).

-opaque transaction() :: #{
    version             := ?ACTUAL_FORMAT_VERSION,
    id                  := id(),
    body                := body(),
    source              := ref(),
    destination         := ref(),
    route               := maybe(route()),
    session_type        := session_type(),
    transfer_type       := transfer_type(),
    activity            => activity(),
    p_transfer          => maybe(p_transfer()),
    session_id          => session_id(),
    status              => status()
}.

-type ref() :: {ref_type(), id()}.

-type ref_type() ::
    source           |
    destination      |
    wallet           .

-type status() ::
    pending          |
    succeeded        |
    {failed, _TODO}  .

-type event() ::
    {created, transaction()}                    |
    {p_transfer, ff_postings_transfer:event()}  |
    {session_started, session_id()}             |
    {session_finished, session_id()}            |
    {status_changed, status()}              .

-type maybe(T)              :: ff_maybe:maybe(T).
-type body()                :: ff_transaction:body().
-type route()               :: ff_transfer_new:route().
-type activity()            ::
    new                      |
    p_transfer_starting      |
    p_transfer_preparing     |
    session_starting         |
    session_polling          |
    p_transfer_finishing     .

-type action()              ::
    next                     |
    undefined                .

-export_type([transaction/0]).
-export_type([event/0]).
-export_type([status/0]).
-export_type([body/0]).
-export_type([action/0]).

-export([id/1]).
-export([transfer_type/1]).
-export([session_type/1]).
-export([body/1]).
-export([p_transfer/1]).
-export([status/1]).
-export([session_id/1]).
-export([route/1]).
-export([activity/1]).

-export([make_ref/2]).

-export([get_empty_session_type/0]).
-export([get_session_type/1]).

-export([create/1]).
-export([process_transaction/1]).
-export([process_failure/2]).

%% Event source

-export([apply_event/2]).
-export([maybe_migrate/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2, with/3]).

%% Internal types

-type id()              :: binary().
-type session_id()      :: id().
-type session_type()    :: empty | withdrawal.
-type p_transfer()      :: ff_postings_transfer:transaction().
-type legacy_event()    :: any().
-type transfer_type()   :: atom().
-type account()         :: ff_account:account().
-type cash_flow_plan()  :: ff_cash_flow:cash_flow_plan().
-type final_cash_flow() :: ff_cash_flow:final_cash_flow().

%% Accessors

-spec id(transaction()) -> id().
id(#{id := V}) ->
    V.

-spec transfer_type(transaction()) -> transfer_type().
transfer_type(#{transfer_type := V}) ->
    V.

-spec session_type(transaction()) -> session_type().
session_type(#{session_type := V}) ->
    V.

-spec body(transaction()) -> body().
body(#{body := V}) ->
    V.

-spec status(transaction()) -> status().
status(#{status := V}) ->
    V.

-spec p_transfer(transaction())  -> maybe(p_transfer()).
p_transfer(#{p_transfer := V}) ->
    V;
p_transfer(_Other) ->
    undefined.

-spec session_id(transaction())  -> maybe(session_id()).
session_id(#{session_id := V}) ->
    V;
session_id(_Other) ->
    undefined.

-spec route(transaction())  -> maybe(route()).
route(#{route := V}) ->
    V.

-spec activity(transaction())  -> maybe(activity()).
activity(#{activity := V}) ->
    V;
activity(_Other) ->
    undefined.

-spec get_empty_session_type() ->
    session_type().

get_empty_session_type() ->
    empty.

-spec get_session_type(withdrawal) ->
    session_type().

get_session_type(withdrawal) ->
    withdrawal.

-spec make_ref(ref_type(), id()) ->
    ref().

make_ref(Type, ID) ->
    {Type, ID}.

%% API

-type create_params() :: #{
    id              := id(),
    body            := body(),
    source          := ref(),
    destination     := ref(),
    route           := route(),
    transfer_type   := transfer_type(),
    session_type    := session_type()
}.

-spec create(create_params()) ->
    {ok, [event()]} |
    {error,
        _PostingsTransferError
    }.

create(#{
    id              := ID,
    body            := Body,
    source          := Source,
    destination     := Destination,
    route           := Route,
    transfer_type   := TransferType,
    session_type    := SessionType
}) ->
    do(fun () ->
        [
            {created, #{
                version       => ?ACTUAL_FORMAT_VERSION,
                id            => ID,
                body          => Body,
                source        => Source,
                destination   => Destination,
                route         => Route,
                transfer_type => TransferType,
                session_type  => SessionType
            }},
            {status_changed, pending}
        ]
    end).

%% ff_transfer_machine_new behaviour

-spec process_transaction(transaction()) ->
    {ok, {ff_transfer_machine_new:action(), [ff_transfer_machine_new:event(event())]}} |
    {error, _Reason}.

process_transaction(Transaction) ->
    process_activity(activity(Transaction), Transaction).

-spec process_failure(any(), transaction()) ->
    {ok, {ff_transfer_machine_new:action(), [ff_transfer_machine_new:event(event())]}} |
    {error, _Reason}.

process_failure(Reason, Transaction) ->
    {ok, ShutdownEvents} = do_process_failure(Reason, Transaction),
    {ok, {undefined,
        ShutdownEvents ++
        [{status_changed, {failed, Reason}}]
    }}.

do_process_failure(_Reason, #{status := pending, activity := Status}) when
    Status =:= p_transfer_starting orelse
    Status =:= p_transfer_preparing
->
    {ok, []};
do_process_failure(_Reason, #{status := pending, activity := session_starting} = Transaction) ->
    do(fun () ->
        unwrap(with(
            p_transfer,
            Transaction,
            fun ff_postings_transfer:cancel/1))
    end);
do_process_failure(Reason, #{status := pending, activity := Status}) when
    Status =:= session_polling orelse
    Status =:= p_transfer_finishing
->
    erlang:error({unprocessable_failure, {step, Status}, Reason});
do_process_failure(_Reason, Transaction) ->
    no_p_transfer = maps:get(p_transfer, Transaction, no_p_transfer),
    {ok, []}.

process_activity(p_transfer_starting, Transaction) ->
    do(fun () ->
        create_p_transfer(Transaction)
    end);
process_activity(p_transfer_preparing, Transaction) ->
    do(fun () ->
        {continue, unwrap(with(
            p_transfer,
            Transaction,
            fun ff_postings_transfer:prepare/1)
        )}
    end);
process_activity(session_starting, Transaction = #{route := Route}) when
    Route =/= undefined
->
    ID = construct_session_id(id(Transaction)),
    do(fun () ->
        {TransferData, SessionParams} = prepare_session_params(
            session_type(Transaction),
            ID,
            Transaction
        ),
        ok = unwrap(ff_withdrawal_session_machine:create(ID, TransferData, SessionParams)),
        {continue, [{session_started, ID}]}
    end);
process_activity(session_starting, Transaction) ->
    do(fun () ->
        Body = body(Transaction),
        WalletID = find_id_in_ref(wallet, Transaction),
        Wallet = ff_wallet_machine:wallet(unwrap(wallet, ff_wallet_machine:get(WalletID))),
        WalletAccount = ff_wallet:account(Wallet),
        valid = unwrap(ff_party:validate_wallet_limits(WalletID, Body, WalletAccount)),
        {continue, [{status_changed, succeeded}]}
    end);
process_activity(session_polling, Transaction) ->
    SessionID = session_id(Transaction),
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
    end);
process_activity(p_transfer_finishing, Transaction) ->
    do(fun () ->
        Fun = get_finish_fun(status(Transaction)),
        {undefined, unwrap(with(
            p_transfer,
            Transaction,
            Fun
        ))}
    end).

get_finish_fun(succeeded) ->
    fun ff_postings_transfer:commit/1;
get_finish_fun({failed, _}) ->
    fun ff_postings_transfer:cancel/1.

create_p_transfer(Transaction = #{route := Route, source := SourceRef, destination := DestinationRef}) when
    Route =/= undefined
->
    WalletID = find_id_in_ref(wallet, Transaction),
    DestinationID = find_id_in_ref(destination, Transaction),
    SourceAccount = get_account_for_ref(SourceRef),
    DestinationAccount = get_account_for_ref(DestinationRef),

    {_Amount, CurrencyID} = body(Transaction),
    #{provider_id := ProviderID} = route(Transaction),

    Provider = unwrap(provider, ff_payouts_provider:get(ProviderID)),
    ProviderAccounts = ff_payouts_provider:accounts(Provider),
    ProviderAccount = maps:get(CurrencyID, ProviderAccounts, undefined),

    Wallet = ff_wallet_machine:wallet(unwrap(wallet, ff_wallet_machine:get(WalletID))),
    PaymentInstitutionID = unwrap(ff_party:get_wallet_payment_institution_id(Wallet)),
    PaymentInstitution = unwrap(ff_payment_institution:get(PaymentInstitutionID)),
    DestinationMachine = unwrap(destination, ff_destination:get_machine(DestinationID)),
    Destination = ff_destination:get(DestinationMachine),
    VS = unwrap(collect_varset(body(Transaction), Wallet, Destination)),
    SystemAccounts = unwrap(ff_payment_institution:compute_system_accounts(PaymentInstitution, VS)),

    SystemAccount = maps:get(CurrencyID, SystemAccounts, #{}),
    SettlementAccount = maps:get(settlement, SystemAccount, undefined),
    SubagentAccount = maps:get(subagent, SystemAccount, undefined),

    ProviderFee = ff_payouts_provider:compute_fees(Provider, VS),

    Terms = unwrap(contract, ff_party:get_contract_terms(Wallet, body(Transaction), ff_time:now())),
    WalletCashFlowPlan = unwrap(cash_flow_plan, ff_party:get_withdrawal_cash_flow_plan(Terms)),

    CashFlowPlan = unwrap(provider_fee, ff_cash_flow:add_fee(WalletCashFlowPlan, ProviderFee)),
    FinalCashFlow = unwrap(cash_flow, finalize_cash_flow(
        CashFlowPlan,
        SourceAccount,
        DestinationAccount,
        SettlementAccount,
        SubagentAccount,
        ProviderAccount,
        body(Transaction)
    )),
    PTransferID = construct_p_transfer_id(id(Transaction)),
    PostingsTransferEvents = unwrap(p_transfer, ff_postings_transfer:create(PTransferID, FinalCashFlow)),
    {continue, [{p_transfer, Ev} || Ev <- PostingsTransferEvents]};
create_p_transfer(Transaction = #{source := Source, destination := Destination}) ->
    SourceAccount = get_account_for_ref(Source),
    DestinationAccount = get_account_for_ref(Destination),

    Constants = #{
        operation_amount => body(Transaction)
    },
    Accounts = #{
        {wallet, sender_source} => SourceAccount,
        {wallet, receiver_settlement} => DestinationAccount
    },

    CashFlowPlan = unwrap(cash_flow_plan,
        ff_party:get_cash_flow_plan(transfer_type(Transaction), #{})),

    FinalCashFlow = unwrap(cash_flow, ff_cash_flow:finalize(CashFlowPlan, Accounts, Constants)),
    PTransferID = construct_p_transfer_id(id(Transaction)),
    PostingsTransferEvents = unwrap(p_transfer, ff_postings_transfer:create(PTransferID, FinalCashFlow)),
    {continue, [{p_transfer, Ev} || Ev <- PostingsTransferEvents]}.

prepare_session_params(empty, _ID, _Transaction) ->
    erlang:error({badtype, <<"Cant create empty session.">>});
prepare_session_params(withdrawal, ID, Transaction) ->
    Body = body(Transaction),
    WalletID = find_id_in_ref(wallet, Transaction),
    DestinationID = find_id_in_ref(destination, Transaction),

    Wallet = ff_wallet_machine:wallet(unwrap(wallet, ff_wallet_machine:get(WalletID))),
    WalletAccount = ff_wallet:account(Wallet),
    Destination = ff_destination:get(
        unwrap(destination, ff_destination:get_machine(DestinationID))
    ),
    DestinationAccount = ff_destination:account(Destination),
    valid = unwrap(ff_party:validate_wallet_limits(WalletID, Body, WalletAccount)),
    #{provider_id := ProviderID} = route(Transaction),

    SenderSt = unwrap(ff_identity_machine:get(ff_account:identity(WalletAccount))),
    ReceiverSt = unwrap(ff_identity_machine:get(ff_account:identity(DestinationAccount))),

    TransferData = #{
        id          => ID,
        cash        => body(Transaction),
        sender      => ff_identity_machine:identity(SenderSt),
        receiver    => ff_identity_machine:identity(ReceiverSt)
    },
    SessionParams = #{
        %% TODO can we here use wallet or source ID?
        destination => DestinationID,
        provider_id => ProviderID
    },
    {TransferData, SessionParams}.

construct_session_id(ID) ->
    ID.

-spec construct_p_transfer_id(id()) -> id().
construct_p_transfer_id(ID) ->
    <<"ff/transaction/", ID/binary>>.

find_id_in_ref(Type, Transaction) ->
    find_id_in_ref(Type, Transaction, undefined).

find_id_in_ref(Type, #{source := {Type, ID}}, ID2) when ID =/= ID2 ->
    ID;
find_id_in_ref(Type, #{destination := {Type, ID}}, ID2) when ID =/= ID2 ->
    ID;
find_id_in_ref(Type, Transaction, ID) ->
    erlang:error({ref_not_found, {Type, Transaction, ID}}).

get_account_for_ref({source, ID}) ->
    Source = ff_source:get(
        unwrap(source, ff_source:get_machine(ID))
    ),
    ff_source:account(Source);
get_account_for_ref({destination, ID}) ->
    Destination = ff_destination:get(
        unwrap(destination, ff_destination:get_machine(ID))
    ),
    ff_destination:account(Destination);
get_account_for_ref({wallet, ID}) ->
    Wallet = ff_wallet_machine:wallet(
        unwrap(wallet, ff_wallet_machine:get(ID))
    ),
    ff_wallet:account(Wallet).

%%

-spec apply_event(event() | legacy_event(), ff_maybe:maybe(transaction())) ->
    {action(), transaction()}.
apply_event(Ev, T) ->
    Transaction = apply_event_(maybe_migrate(Ev, maybe_transfer_type(T)), T),
    {get_action_from_activity(activity(Transaction)), Transaction}.

-spec apply_event_(event(), ff_maybe:maybe(transaction())) ->
    transaction().
apply_event_({created, T}, undefined) ->
    set_activity(p_transfer_starting, T);
apply_event_({status_changed, S}, T) ->
    set_activity(get_activity_from_status(S), maps:put(status, S, T));
apply_event_({p_transfer, Ev}, T = #{p_transfer := PT}) ->
    PTransfer = ff_postings_transfer:apply_event(Ev, PT),
    set_activity(get_activity_from_p_status(PTransfer), T#{p_transfer := PTransfer});
apply_event_({p_transfer, Ev}, T0) ->
    {_, T1} = apply_event({p_transfer, Ev}, T0#{p_transfer => undefined}),
    T1;
apply_event_({session_started, S}, T) ->
    set_activity(session_polling, maps:put(session_id, S, T));
apply_event_({session_finished, S}, T = #{session_id := S}) ->
    T.

maybe_transfer_type(undefined) ->
    undefined;
maybe_transfer_type(T) ->
    transfer_type(T).

set_activity(undefined, T) ->
    T;
set_activity(Activity, T) ->
    maps:put(activity, Activity, T).

get_activity_from_p_status(#{status := created}) ->
    p_transfer_preparing;
get_activity_from_p_status(#{status := prepared}) ->
    session_starting;
get_activity_from_p_status(_) ->
    undefined.

get_activity_from_status(succeeded) ->
    p_transfer_finishing;
get_activity_from_status({failed, _}) ->
    p_transfer_finishing;
get_activity_from_status(_) ->
    undefined.

get_action_from_activity(p_transfer_starting) ->
    next;
get_action_from_activity(_) ->
    undefined.

-spec maybe_migrate(event() | legacy_event(), transfer_type() | undefined) ->
    event().
% Actual events
maybe_migrate(Ev = {created, #{version := ?ACTUAL_FORMAT_VERSION}}, _) ->
    Ev;
maybe_migrate({p_transfer, PEvent}, EventType) ->
    {p_transfer, ff_postings_transfer:maybe_migrate(PEvent, EventType)};
% Old events
maybe_migrate({transaction, PTransferEv}, EventType) ->
    maybe_migrate({p_transfer, PTransferEv}, EventType);
% Other events
maybe_migrate(Ev, _) ->
    Ev.

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
