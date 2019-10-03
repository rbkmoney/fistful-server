%%%
%%% Financial transaction between accounts
%%%
%%%  - Rename to `ff_posting_plan`?
%%%

-module(ff_transaction).

-include_lib("shumpune_proto/include/shumpune_shumpune_thrift.hrl").

%%

-type id()       :: shumpune_shumpune_thrift:'PlanID'().
-type account()  :: shumpune_shumpune_thrift:'AccountID'().
-type clock()    :: shumpune_shumpune_thrift:'Clock'().
-type amount()   :: dmsl_domain_thrift:'Amount'().
-type body()     :: ff_cash:cash().
-type posting()  :: {account(), account(), body()}.
-type balance()  :: {ff_indef:indef(amount()), ff_currency:id()}.
-type affected() :: #{account() => balance()}.

-export_type([id/0]).
-export_type([body/0]).
-export_type([account/0]).
-export_type([posting/0]).
-export_type([clock/0]).

%% TODO
%%  - Module name is misleading then
-export([balance/2]).
-export([latest_clock/0]).

-export([prepare/2]).
-export([commit/2]).
-export([cancel/2]).

%%

-spec balance(account(), clock()) ->
    {ok, balance()}.

balance(Account, Clock) ->
    AccountID = ff_account:accounter_account_id(Account),
    Currency = ff_account:currency(Account),
    {ok, Balance} = get_balance_by_id(AccountID, Clock),
    {ok, build_account_balance(Balance, Currency)}.

-spec latest_clock() -> clock().
latest_clock() ->
    {latest, #shumpune_LatestClock{}}.

-spec prepare(id(), [posting()]) ->
    {ok, affected()}.

prepare(ID, Postings) ->
    hold(encode_plan_change(ID, Postings)).

-spec commit(id(), [posting()]) ->
    {ok, affected()}.

commit(ID, Postings) ->
    commit_plan(encode_plan(ID, Postings)).

-spec cancel(id(), [posting()]) ->
    {ok, affected()}.

cancel(ID, Postings) ->
    rollback_plan(encode_plan(ID, Postings)).

%% Woody stuff

get_balance_by_id(ID, Clock) ->
    case call('GetBalanceByID', [ID, Clock]) of
        {ok, Balance} ->
            {ok, Balance};
        {exception, Unexpected} ->
            error(Unexpected)
    end.

hold(PlanChange) ->
    case call('Hold', [PlanChange]) of
        {ok, Clock} ->
            {ok, Clock};
        {exception, Unexpected} ->
            error(Unexpected)
    end.

commit_plan(Plan) ->
    case call('CommitPlan', [Plan]) of
        {ok, Clock} ->
            {ok, Clock};
        {exception, Unexpected} ->
            error(Unexpected)
    end.

rollback_plan(Plan) ->
    case call('RollbackPlan', [Plan]) of
        {ok, Clock} ->
            {ok, Clock};
        {exception, Unexpected} ->
            error(Unexpected)
    end.

call(Function, Args) ->
    Service = {shumpune_shumpune_thrift, 'Accounter'},
    ff_woody_client:call(accounter, {Service, Function, Args}).

encode_plan_change(ID, Postings) ->
    #shumpune_PostingPlanChange{
        id    = ID,
        batch = encode_batch(Postings)
    }.

encode_plan(ID, Postings) ->
    #shumpune_PostingPlan{
        id         = ID,
        batch_list = [encode_batch(Postings)]
    }.

encode_batch(Postings) ->
    #shumpune_PostingBatch{
        id       = 1, % TODO
        postings = [
            encode_posting(Source, Destination, Body)
                || {Source, Destination, Body} <- Postings
        ]
    }.

encode_posting(Source, Destination, {Amount, Currency}) ->
    #shumpune_Posting{
        from_id           = Source,
        to_id             = Destination,
        amount            = Amount,
        currency_sym_code = Currency,
        description       = <<"TODO">>
    }.

build_account_balance(
    #shumpune_Balance{
        own_amount = Own,
        max_available_amount = MaxAvail,
        min_available_amount = MinAvail
    },
    Currency) ->
    {ff_indef:new(MinAvail, Own, MaxAvail), Currency}.
