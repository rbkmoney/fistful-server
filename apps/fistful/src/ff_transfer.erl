%%%
%%% Tranfer
%%%
%%% TODOs
%%%
%%%  - We must synchronise any transfers on wallet machine, as one may request
%%%    us to close wallet concurrently. Moreover, we should probably check any
%%%    limits there too.
%%%  - Well, we will need `cancel` soon too.
%%%  - What if we get rid of some failures in `prepare`, specifically those
%%%    which related to wallet blocking / suspension? It would be great to get
%%%    rid of the `wallet closed` failure but I see no way to do so.
%%%

-module(ff_transfer).

-include_lib("dmsl/include/dmsl_accounter_thrift.hrl").

-type body()           :: {integer(), ff_currency:id()}.
-type wallet_id()      :: ff_wallet_machine:id().
-type transaction_id() :: binary().

-type status() ::
    created   |
    prepared  |
    committed |
    cancelled .

-type transfer() :: #{
    source      := wallet_id(),
    destination := wallet_id(),
    trxid       := transaction_id(),
    body        := body(),
    status      := status()
}.

-export_type([body/0]).
-export_type([transfer/0]).
-export_type([status/0]).

-export([source/1]).
-export([destination/1]).
-export([trxid/1]).
-export([body/1]).
-export([status/1]).

-export([create/4]).

-export([prepare/1]).
-export([commit/1]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%%

-spec source(transfer()) -> wallet_id().
-spec destination(transfer()) -> wallet_id().
-spec trxid(transfer()) -> transaction_id().
-spec body(transfer()) -> body().
-spec status(transfer()) -> status().

source(#{source := V}) -> V.
destination(#{destination := V}) -> V.
trxid(#{trxid := V}) -> V.
body(#{body := V}) -> V.
status(#{status := V}) -> V.

%%

-spec create(wallet_id(), wallet_id(), transaction_id(), body()) ->
    {ok, transfer()} |
    {error,
        {source | destination,
            notfound |
            {inaccessible, blocked | suspended} |
            {currency, invalid} |
            {provider, invalid}
        }
    }.

create(SourceID, DestinationID, TrxID, Body = {_, Currency}) ->
    do(fun () ->
        Source            = unwrap(source, get_wallet(SourceID)),
        valid             = unwrap(source, validate_currency(Source, Currency)),
        Destination       = unwrap(destination, get_wallet(DestinationID)),
        valid             = unwrap(destination, validate_currency(Destination, Currency)),
        {ok, SrcIdentity} = ff_identity_machine:get(ff_wallet:identity(Source)),
        {ok, DstIdentity} = ff_identity_machine:get(ff_wallet:identity(Destination)),
        Provider          = ff_identity:provider(SrcIdentity),
        valid             = unwrap(destination, do(fun () ->
            unwrap(provider, validate(Provider, ff_identity:provider(DstIdentity)))
        end)),
        #{
            source      => SourceID,
            destination => DestinationID,
            body        => Body,
            trxid       => TrxID,
            status      => created
        }
    end).

get_wallet(WalletID) ->
    do(fun () ->
        Wallet     = unwrap(ff_wallet_machine:get(WalletID)),
        accessible = unwrap(ff_wallet:is_accessible(Wallet)),
        Wallet
    end).

validate_currency(Wallet, Currency) ->
    do(fun () ->
        valid = unwrap(currency, validate(Currency, ff_wallet:currency(Wallet)))
    end).

validate(Currency, Currency) ->
    {ok, valid};
validate(_, _) ->
    {error, invalid}.

%%

-spec prepare(transfer()) ->
    {ok, transfer()} |
    {error,
        status() |
        {source | destination,
            notfound |
            {inaccessible, blocked | suspended}
        }
    }.

prepare(Transfer = #{status := created}) ->
    do(fun () ->
        Source      = unwrap(source, get_wallet(source(Transfer))),
        Destination = unwrap(destination, get_wallet(destination(Transfer))),
        PlanChange  = construct_plan_change(Source, Destination, trxid(Transfer), body(Transfer)),
        _Affected   = hold(PlanChange),
        Transfer#{status := prepared}
    end);
prepare(Transfer = #{status := prepared}) ->
    {ok, Transfer};
prepare(#{status := Status}) ->
    {error, Status}.

%%

-spec commit(transfer()) ->
    {ok, transfer()} |
    {error, status()}.

commit(Transfer = #{status := prepared}) ->
    do(fun () ->
        {ok, Source}      = ff_wallet_machine:get_wallet(source(Transfer)),
        {ok, Destination} = ff_wallet_machine:get_wallet(destination(Transfer)),
        Plan              = construct_plan(Source, Destination, trxid(Transfer), body(Transfer)),
        _Affected         = commit_plan(Plan),
        Transfer#{status := committed}
    end);
commit(Transfer = #{status := committed}) ->
    {ok, Transfer};
commit(#{status := Status}) ->
    {error, Status}.

%% Woody stuff

hold(PlanChange) ->
    case call('Hold', [PlanChange]) of
        {ok, #accounter_PostingPlanLog{affected_accounts = Affected}} ->
            {ok, Affected};
        {error, Unexpected} ->
            error(Unexpected)
    end.

commit_plan(Plan) ->
    case call('CommitPlan', [Plan]) of
        {ok, #accounter_PostingPlanLog{affected_accounts = Affected}} ->
            {ok, Affected};
        {error, Unexpected} ->
            error(Unexpected)
    end.

call(Function, Args) ->
    Service = {dmsl_accounter_thrift, 'Accounter'},
    ff_woody_client:call(accounter, {Service, Function, Args}).

construct_plan_change(Source, Destination, Body, TrxID) ->
    #accounter_PostingPlanChange{
        id    = TrxID,
        batch = construct_batch(Source, Destination, Body)
    }.

construct_plan(Source, Destination, Body, TrxID) ->
    #accounter_PostingPlan{
        id         = TrxID,
        batch_list = [construct_batch(Source, Destination, Body)]
    }.

construct_batch(Source, Destination, Body) ->
    #accounter_PostingBatch{
        id       = 1, % TODO
        postings = [construct_posting(Source, Destination, Body)]
    }.

construct_posting(Source, Destination, {Amount, Currency}) ->
    #accounter_Posting{
        from_id           = ff_wallet:account(Source),
        to_id             = ff_wallet:account(Destination),
        amount            = Amount,
        currency_sym_code = Currency,
        description       = <<"TODO">>
    }.
