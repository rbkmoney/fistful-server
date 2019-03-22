%%%
%%% Deposit
%%%

-module(ff_deposit).

-type id()          :: ff_transfer_machine:id().
-type source_id()   :: ff_source:id().
-type wallet_id()   :: ff_wallet:id().

-type deposit() :: ff_transfer:transfer(transfer_params()).
-type transfer_params() :: #{
    source_id := source_id(),
    wallet_id := wallet_id(),
    wallet_account := account(),
    source_account := account(),
    wallet_cash_flow_plan := cash_flow_plan()
}.

-type machine() :: ff_transfer_machine:st(transfer_params()).
-type events()  :: ff_transfer_machine:events().
-type event()   :: ff_transfer_machine:event(ff_transfer:event(transfer_params(), route())).
-type route()   :: ff_transfer:route(none()).

-export_type([deposit/0]).
-export_type([machine/0]).
-export_type([transfer_params/0]).
-export_type([events/0]).
-export_type([event/0]).
-export_type([route/0]).

%% ff_transfer_machine behaviour
-behaviour(ff_transfer_machine).
-export([process_transfer/1]).
-export([process_failure/2]).
-export([process_call/2]).

%% Accessors

-export([wallet_id/1]).
-export([source_id/1]).
-export([id/1]).
-export([body/1]).
-export([status/1]).
-export([external_id/1]).

%% API
-export([create/3]).
-export([get/1]).
-export([get_machine/1]).
-export([events/2]).
-export([revert/3]).

%% Event source

-export([maybe_migrate/1]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2, valid/2]).

%% Internal types

-type account() :: ff_account:account().
-type process_result() :: {ff_transfer_machine:action(), [event()]}.
-type cash_flow_plan() :: ff_cash_flow:cash_flow_plan().

%% Accessors

-spec wallet_id(deposit())                    -> source_id().
-spec source_id(deposit())                    -> wallet_id().
-spec id(ff_transfer:transfer())              -> ff_transfer:id().
-spec body(ff_transfer:transfer())            -> ff_transfer:body().
-spec status(ff_transfer:transfer())          -> ff_transfer:status().
-spec params(ff_transfer:transfer())          -> transfer_params().

wallet_id(T)       -> maps:get(wallet_id, ff_transfer:params(T)).
source_id(T)       -> maps:get(source_id, ff_transfer:params(T)).
id(T)              -> ff_transfer:id(T).
body(T)            -> ff_transfer:body(T).
status(T)          -> ff_transfer:status(T).
params(T)          -> ff_transfer:params(T).

-spec external_id(deposit()) ->
    id() | undefined.
external_id(T)     -> ff_transfer:external_id(T).

%%

-define(NS, 'ff/deposit_v1').

-type ctx()    :: ff_ctx:ctx().
-type params() :: #{
    source_id   := ff_source:id(),
    wallet_id   := ff_wallet_machine:id(),
    body        := ff_transaction:body(),
    external_id => id()
}.

-spec create(id(), params(), ctx()) ->
    ok |
    {error,
        {source, notfound | unauthorized} |
        {destination, notfound} |
        {provider, notfound} |
        exists |
        _TransferError
    }.

create(ID, Args = #{source_id := SourceID, wallet_id := WalletID, body := Body}, Ctx) ->
    do(fun() ->
        Source = ff_source:get(unwrap(source, ff_source:get_machine(SourceID))),
        Wallet = ff_wallet_machine:wallet(unwrap(destination, ff_wallet_machine:get(WalletID))),
        valid =  unwrap(ff_party:validate_deposit_creation(Wallet, Body)),
        ok = unwrap(source, valid(authorized, ff_source:status(Source))),
        Params = #{
            handler     => ?MODULE,
            body        => Body,
            params      => #{
                wallet_id             => WalletID,
                source_id             => SourceID,
                wallet_account        => ff_wallet:account(Wallet),
                source_account        => ff_source:account(Source),
                wallet_cash_flow_plan => #{
                    postings => [
                        #{
                            sender   => {wallet, sender_source},
                            receiver => {wallet, receiver_settlement},
                            volume   => {share, {{1, 1}, operation_amount, default}}
                        }
                    ]
                }
            },
            external_id => maps:get(external_id, Args, undefined)
        },
        unwrap(ff_transfer_machine:create(?NS, ID, Params, Ctx))
    end).

-spec get(machine()) ->
    deposit().

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

-spec revert(id(), ff_transfer:body(), binary() | undefined) ->
    {ok, ff_reposit:reposit()}  |
    {error, _TransferError}.
revert(ID, Body, Reason) ->
    Params = #{
        id => ID,
        body => Body,
        reason => maybe_reason(Reason)
    },
    do(fun() -> unwrap(ff_transfer_machine:revert(?NS, Params)) end).

maybe_reason(undefined) ->
    <<"">>;
maybe_reason(Reason) ->
    Reason.

%% ff_transfer_machine behaviour

-spec process_transfer(ff_transfer:transfer()) ->
    {ok, process_result()} |
    {error, _Reason}.

process_transfer(Transfer) ->
    Activity = deduce_activity(Transfer),
    do_process_transfer(Activity, Transfer).

-spec process_failure(any(), ff_transfer:transfer()) ->
    {ok, process_result()} |
    {error, _Reason}.

process_failure(Reason, Transfer) ->
    ff_transfer:process_failure(Reason, Transfer).

-spec process_call(any(), ff_transfer:transfer()) ->
    {ok, process_result()} |
    {error, _Reason}.

process_call({revert, Body, Reason}, Transfer) ->
    #{
        source_id := SourceID,
        wallet_id := WalletID,
        wallet_account := WalletAccount,
        source_account := SourceAccount
    } = params(Transfer),
    Params = #{
        deposit_id      => id(Transfer),
        source_id       => SourceID,
        wallet_id       => WalletID,
        body            => Body,
        reason          => Reason
    },
    do(fun () ->
        DepositBody = body(Transfer),

        ok = unwrap(validate_cash(Body, DepositBody)),
        ok = unwrap(validate_deposit_state(Transfer)),

        Reposits = ff_transfer:reposits(Transfer),

        ok = unwrap(validate_reposit_state(Reposits)),

        RepositID = ff_reposit:next_id(Reposits),
        RepositEvents = unwrap(ff_reposit:create(RepositID, Params)),
        Reposit = ff_reposit:collapse(RepositEvents),
        PTransferID = ff_reposit:construct_p_transfer_id(Reposit),
        PostingsTransferEvents = create_p_transfer_(PTransferID, WalletAccount, SourceAccount, Body, Transfer),
        {continue,
            [{status_changed, pending}] ++
            ff_transfer:wrap_events(reposit, RepositEvents) ++
            PostingsTransferEvents
        }
    end).

%% Internals

-type activity() ::
    p_transfer_start         |
    finish                   |
    idle                     .

% TODO: Move activity to ff_transfer
-spec deduce_activity(deposit()) ->
    activity().
deduce_activity(Transfer) ->
    Params = #{
        p_transfer => ff_transfer:p_transfer(Transfer),
        status => status(Transfer)
    },
    do_deduce_activity(Params).

do_deduce_activity(#{status := pending, p_transfer := undefined}) ->
    p_transfer_start;
do_deduce_activity(#{status := pending, p_transfer := #{status := prepared}}) ->
    finish_him;
do_deduce_activity(_Other) ->
    idle.

do_process_transfer(p_transfer_start, Transfer) ->
    create_p_transfer(Transfer);
do_process_transfer(finish_him, Transfer) ->
    finish_transfer(Transfer);
do_process_transfer(idle, Transfer) ->
    ff_transfer:process_transfer(Transfer).

-spec create_p_transfer(ff_transfer:transfer()) ->
    {ok, process_result()} |
    {error, _Reason}.
create_p_transfer(Transfer) ->
    #{
        wallet_account := WalletAccount,
        source_account := SourceAccount
    } = params(Transfer),
    do(fun () ->
        PTransferID = construct_p_transfer_id(id(Transfer)),
        {continue, create_p_transfer_(PTransferID, SourceAccount, WalletAccount, body(Transfer), Transfer)}
    end).

-spec finish_transfer(ff_transfer:transfer()) ->
    {ok, {ff_transfer_machine:action(), [ff_transfer_machine:event(ff_transfer:event())]}} |
    {error, _Reason}.
finish_transfer(Transfer) ->
    Action = ff_transfer:action(Transfer),
    Body = get_body_for_action(Action, Transfer),
    #{
        wallet_id := WalletID,
        wallet_account := WalletAccount
    } = params(Transfer),
    do(fun () ->
        valid = unwrap(ff_party:validate_wallet_limits(WalletID, Body, WalletAccount)),
        {continue, get_success_events_for_action(Action)}
    end).

get_body_for_action(undefined, Transfer) ->
    body(Transfer);
get_body_for_action(revert, Transfer) ->
    ff_reposit:body(ff_transfer:reposit(Transfer)).

get_success_events_for_action(undefined) ->
    [{status_changed, succeeded}];
get_success_events_for_action(revert) ->
    [{status_changed, {reverted, <<"reverted">>}}] ++
    ff_transfer:wrap_events(reposit, ff_reposit:update_status(succeeded)).

-spec construct_p_transfer_id(id()) -> id().
construct_p_transfer_id(ID) ->
    <<"ff/deposit/", ID/binary>>.

-spec maybe_migrate(ff_transfer:event() | ff_transfer:legacy_event()) ->
    ff_transfer:event().
maybe_migrate(Ev) ->
    ff_transfer:maybe_migrate(Ev, deposit).

create_p_transfer_(PTransferID, From, To, Body, Transfer) ->
    #{
        wallet_cash_flow_plan := CashFlowPlan
    } = params(Transfer),
    Constants = #{
        operation_amount => Body
    },
    Accounts = #{
        {wallet, sender_source} => From,
        {wallet, receiver_settlement} => To
    },
    FinalCashFlow = unwrap(cash_flow, ff_cash_flow:finalize(CashFlowPlan, Accounts, Constants)),
    PostingsTransferEvents = unwrap(p_transfer, ff_postings_transfer:create(PTransferID, FinalCashFlow)),
    [{p_transfer, Ev} || Ev <- PostingsTransferEvents].

validate_cash(Checked, CheckWith) ->
    case ff_cash:validate_cash_to_cash(Checked, CheckWith) of
        valid ->
            ok;
        Result ->
            Result
    end.

validate_deposit_state(Transfer) ->
    case ff_transfer:status(Transfer) of
        succeeded ->
            ok;
        {reverted, _} ->
            ok;
        _Result ->
            {error, {not_permitted, <<"Wrong deposit state">>}}
    end.

validate_reposit_state(Reposits) when is_list(Reposits) ->
    case ff_reposit:status(ff_reposit:get_current(Reposits)) of
        succeeded ->
            ok;
        _Result ->
            {error, {not_permitted, <<"Previous reposit not finished">>}}
    end;
validate_reposit_state(_) ->
    ok.
