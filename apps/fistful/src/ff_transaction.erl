%%%
%%% Financial transaction between accounts
%%%
%%%  - Rename to `ff_posting_plan`?
%%%

-module(ff_transaction).

-include_lib("dmsl/include/dmsl_accounter_thrift.hrl").

%%

-type id()       :: dmsl_accounter_thrift:'PlanID'().
-type account()  :: dmsl_accounter_thrift:'AccountID'().
-type amount()   :: dmsl_domain_thrift:'Amount'().
-type body()     :: {amount(), ff_currency:id()}.
-type posting()  :: {account(), account(), body()}.
-type balance()  :: {ff_indef:indef(amount()), ff_currency:id()}.
-type affected() :: #{account() => balance()}.

-export_type([id/0]).
-export_type([body/0]).
-export_type([account/0]).

%% TODO
%%  - Module name is misleading then
-export([balance/1]).

-export([prepare/2]).
-export([commit/2]).
-export([cancel/2]).

%%

-spec balance(account()) ->
    {ok, balance()}.

balance(Account) ->
    get_account_by_id(Account).

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

get_account_by_id(ID) ->
    case call('GetAccountByID', [ID]) of
        {ok, Account} ->
            {ok, decode_account_balance(Account)};
        {exception, Unexpected} ->
            error(Unexpected)
    end.

hold(PlanChange) ->
    case call('Hold', [PlanChange]) of
        {ok, #accounter_PostingPlanLog{affected_accounts = Affected}} ->
            {ok, decode_affected(Affected)};
        {exception, Unexpected} ->
            error(Unexpected)
    end.

commit_plan(Plan) ->
    case call('CommitPlan', [Plan]) of
        {ok, #accounter_PostingPlanLog{affected_accounts = Affected}} ->
            {ok, decode_affected(Affected)};
        {exception, Unexpected} ->
            error(Unexpected)
    end.

rollback_plan(Plan) ->
    case call('RollbackPlan', [Plan]) of
        {ok, #accounter_PostingPlanLog{affected_accounts = Affected}} ->
            {ok, decode_affected(Affected)};
        {exception, Unexpected} ->
            error(Unexpected)
    end.

call(Function, Args) ->
    Service = {dmsl_accounter_thrift, 'Accounter'},
    ff_woody_client:call(accounter, {Service, Function, Args}).

encode_plan_change(ID, Postings) ->
    #accounter_PostingPlanChange{
        id    = ID,
        batch = encode_batch(Postings)
    }.

encode_plan(ID, Postings) ->
    #accounter_PostingPlan{
        id         = ID,
        batch_list = [encode_batch(Postings)]
    }.

encode_batch(Postings) ->
    #accounter_PostingBatch{
        id       = 1, % TODO
        postings = [
            encode_posting(Source, Destination, Body)
                || {Source, Destination, Body} <- Postings
        ]
    }.

encode_posting(Source, Destination, {Amount, Currency}) ->
    #accounter_Posting{
        from_id           = Source,
        to_id             = Destination,
        amount            = Amount,
        currency_sym_code = Currency,
        description       = <<"TODO">>
    }.

decode_affected(M) ->
    maps:map(fun (_, A) -> decode_account_balance(A) end, M).

decode_account_balance(#accounter_Account{
    own_amount = Own,
    max_available_amount = MaxAvail,
    min_available_amount = MinAvail,
    currency_sym_code = Currency
}) ->
    {ff_indef:new(MinAvail, Own, MaxAvail), Currency}.
