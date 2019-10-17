%%%
%%% Tranfer
%%%
%%% TODOs
%%%
%%%  - We must synchronise any transfers on wallet machine, as one may request
%%%    us to close wallet concurrently. Moreover, we should probably check any
%%%    limits there too.
%%%  - What if we get rid of some failures in `prepare`, specifically those
%%%    which related to wallet blocking / suspension? It would be great to get
%%%    rid of the `wallet closed` failure but I see no way to do so.
%%%

-module(ff_postings_transfer).

-type final_cash_flow()  :: ff_cash_flow:final_cash_flow().
-type clock()            :: ff_clock:clock().

-type status() ::
    created   |
    prepared  |
    committed |
    cancelled .

-type transfer() :: #{
    id                := id(),
    final_cash_flow   := final_cash_flow(),
    status            => status(),
    clock             => clock()
}.

-type event() ::
    {created, transfer()} |
    {clock_updated, clock()} |
    {status_changed, status()}.

-export_type([transfer/0]).
-export_type([final_cash_flow/0]).
-export_type([status/0]).
-export_type([event/0]).

-export([id/1]).
-export([final_cash_flow/1]).
-export([status/1]).
-export([clock/1]).

-export([create/2]).
-export([prepare/1]).
-export([commit/1]).
-export([cancel/1]).

%% Event source

-export([apply_event/2]).
-export([maybe_migrate/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2, valid/2]).

%% Internal types

-type id()       :: ff_transaction:id().
-type account()  :: ff_account:account().

%%

-spec id(transfer()) ->
    id().
-spec final_cash_flow(transfer()) ->
    final_cash_flow().
-spec status(transfer()) ->
    status().
-spec clock(transfer()) ->
    clock().

id(#{id := V}) ->
    V.
final_cash_flow(#{final_cash_flow := V}) ->
    V.
status(#{status := V}) ->
    V.
clock(#{clock := V}) ->
    V;
clock(_) ->
    ff_clock:latest_clock().

%%

-spec create(id(), final_cash_flow()) ->
    {ok, [event()]} |
    {error,
        empty |
        {account, ff_party:inaccessibility()} |
        {currency, ff_currency:id()} |
        {provider, id()}
    }.

create(_TrxID, #{postings := []}) ->
    {error, empty};
create(ID, CashFlow) ->
    do(fun () ->
        Accounts   = ff_cash_flow:gather_used_accounts(CashFlow),
        valid      = validate_currencies(Accounts),
        valid      = validate_identities(Accounts),
        accessible = validate_accessible(Accounts),
        [
            {created, #{
                id        => ID,
                final_cash_flow => CashFlow
            }},
            {status_changed,
                created
            }
        ]
    end).

validate_accessible(Accounts) ->
    _ = [accessible = unwrap(account, ff_account:is_accessible(A)) || A <- Accounts],
    accessible.

validate_currencies([A0 | Accounts]) ->
    Currency = ff_account:currency(A0),
    _ = [ok = unwrap(currency, valid(Currency, ff_account:currency(A))) || A <- Accounts],
    valid.

validate_identities([A0 | Accounts]) ->
    {ok, IdentitySt} = ff_identity_machine:get(ff_account:identity(A0)),
    Identity0 = ff_identity_machine:identity(IdentitySt),
    ProviderID0 = ff_identity:provider(Identity0),
    _ = [
        ok = unwrap(provider, valid(ProviderID0, ff_identity:provider(ff_identity_machine:identity(Identity)))) ||
            Account <- Accounts,
            {ok, Identity} <- [ff_identity_machine:get(ff_account:identity(Account))]
    ],
    valid.

%%

-spec prepare(transfer()) ->
    {ok, [event()]} |
    {error,
        {status, committed | cancelled}
    }.

prepare(Transfer = #{status := created}) ->
    ID = id(Transfer),
    CashFlow = final_cash_flow(Transfer),
    do(fun () ->
        Clock = unwrap(ff_transaction:prepare(ID, construct_trx_postings(CashFlow))),
        [{clock_updated, Clock}, {status_changed, prepared}]
    end);
prepare(#{status := prepared}) ->
    {ok, []};
prepare(#{status := Status}) ->
    {error, Status}.

%% TODO
% validate_balances(Affected) ->
%     {ok, valid}.

%%

-spec commit(transfer()) ->
    {ok, [event()]} |
    {error, {status, created | cancelled}}.

commit(Transfer = #{status := prepared}) ->
    ID = id(Transfer),
    CashFlow = final_cash_flow(Transfer),
    do(fun () ->
        Clock = unwrap(ff_transaction:commit(ID, construct_trx_postings(CashFlow))),
        [{clock_updated, Clock}, {status_changed, committed}]
    end);
commit(#{status := committed}) ->
    {ok, []};
commit(#{status := Status}) ->
    {error, Status}.

%%

-spec cancel(transfer()) ->
    {ok, [event()]} |
    {error, {status, created | committed}}.

cancel(Transfer = #{status := prepared}) ->
    ID = id(Transfer),
    CashFlow = final_cash_flow(Transfer),
    do(fun () ->
        Clock = unwrap(ff_transaction:cancel(ID, construct_trx_postings(CashFlow))),
        [{clock_updated, Clock}, {status_changed, cancelled}]
    end);
cancel(#{status := cancelled}) ->
    {ok, []};
cancel(#{status := Status}) ->
    {error, {status, Status}}.

%%

-spec apply_event(event(), ff_maybe:maybe(account())) ->
    account().

apply_event({created, Transfer}, undefined) ->
    Transfer;
apply_event({clock_updated, Clock}, Transfer) ->
    Transfer#{clock => Clock};
apply_event({status_changed, S}, Transfer) ->
    Transfer#{status => S}.

%%

-spec construct_trx_postings(final_cash_flow()) ->
    [ff_transaction:posting()].

construct_trx_postings(#{postings := Postings}) ->
    lists:map(fun construct_trx_posting/1, Postings).

-spec construct_trx_posting(ff_cash_flow:final_posting()) ->
    ff_transaction:posting().

construct_trx_posting(Posting) ->
    #{
        sender := #{account := Sender},
        receiver := #{account := Receiver},
        volume := Volume
    } = Posting,
    SenderAccount = ff_account:accounter_account_id(Sender),
    ReceiverAccount = ff_account:accounter_account_id(Receiver),
    {SenderAccount, ReceiverAccount, Volume}.

%% Event migrations
-spec maybe_migrate(any(), withdrawal | deposit) -> event().
% Actual events
maybe_migrate({created, #{final_cash_flow := CashFlow} = EvBody}, EvType) ->
    {created, EvBody#{final_cash_flow => maybe_migrate_cash_flow(CashFlow, EvType)}};
% Old events
maybe_migrate({created, #{postings := Postings} = Transfer}, EvType) ->
    #{
        id := ID,
        postings := Postings
    } = Transfer,
    CashFlowPostings = [
        #{sender => #{account => S}, receiver => #{account => D}, volume => B}
        || {S, D, B} <- Postings
    ],
    maybe_migrate({created, #{
        id              => ID,
        final_cash_flow => #{
            postings => CashFlowPostings
        }
    }}, EvType);
% Other events
maybe_migrate(Ev, _) ->
    Ev.

maybe_migrate_cash_flow(#{postings := CashFlowPostings} = CashFlow, EvType) ->
    NewPostings = [
        maybe_migrate_posting(CashFlowPosting, EvType)
        || CashFlowPosting <- CashFlowPostings
    ],
    CashFlow#{postings => NewPostings}.

% Some cashflow in early withdrawals has been created with binary accounter_account_id
maybe_migrate_posting(#{
    sender := #{accounter_account_id := SenderAcc} = Sender
} = Posting, EvType) when is_binary(SenderAcc) ->
    maybe_migrate_posting(Posting#{
        sender := Sender#{
            accounter_account_id := erlang:binary_to_integer(SenderAcc)
        }
    }, EvType);
maybe_migrate_posting(#{
    receiver := #{accounter_account_id := ReceiverAcc} = Receiver
} = Posting, EvType) when is_binary(ReceiverAcc) ->
    maybe_migrate_posting(Posting#{
        receiver := Receiver#{
            accounter_account_id := erlang:binary_to_integer(ReceiverAcc)
        }
    }, EvType);
maybe_migrate_posting(#{receiver := Receiver, sender := Sender} = Posting, EvType) ->
    Posting#{
        receiver := maybe_migrate_final_account(Receiver, receiver, EvType),
        sender := maybe_migrate_final_account(Sender, sender, EvType)
    }.

maybe_migrate_final_account(Account = #{type := _Type}, _, _) ->
    Account;
maybe_migrate_final_account(Account, receiver, withdrawal) ->
    Account#{type => {wallet, receiver_destination}};
maybe_migrate_final_account(Account, receiver, deposit) ->
    Account#{type => {wallet, receiver_settlement}};
maybe_migrate_final_account(Account, sender, withdrawal) ->
    Account#{type => {wallet, sender_settlement}};
maybe_migrate_final_account(Account, sender, deposit) ->
    Account#{type => {wallet, sender_source}}.

