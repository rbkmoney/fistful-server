-module(ff_cash_flow).

-export([gather_used_accounts/1]).
-export([finalize/3]).
-export([decode_plan/1]).

%% Domain types
-type plan_posting() :: #{
    sender := plan_account(),
    receiver := plan_account(),
    volume := plan_volume(),
    details => binary()
}.
-type plan_volume() ::
    {fixed, cash()} |
    {share, {rational(), plan_constant(), rounding_method()}} |
    {product, plan_operation()}.

-type plan_constant() ::
    operation_amount.
-type plan_operation() ::
    {min_of, [plan_volume()]} |
    {max_of, [plan_volume()]}.

-type rational() :: {P :: integer(), Q :: integer()}.
-type rounding_method() ::
    default |
    round_half_towards_zero |  % https://en.wikipedia.org/wiki/Rounding#Round_half_towards_zero
    round_half_away_from_zero. % https://en.wikipedia.org/wiki/Rounding#Round_half_away_from_zero

-type cash_flow_plan() :: #{
    postings := [plan_posting()]
}.
-type account_mapping() :: #{
    plan_account() => final_account()
}.
-type constant_mapping() :: #{
    operation_amount => cash()
}.

-type final_posting()  :: #{
    sender := final_account(),
    receiver := final_account(),
    volume := cash(),
    details => binary()
}.
-type final_cash_flow() :: #{
    postings := [final_posting()]
}.

-type plan_account() ::
    {wallet, sender_source} |
    {wallet, sender_settlement} |
    {wallet, receiver_settlement} |
    {wallet, receiver_destination} |
    {system, settlement} |
    {provider, settlement}.
-type final_account() :: #{
    account := account(),
    type => plan_account()
}.

-export_type([plan_volume/0]).
-export_type([plan_constant/0]).
-export_type([plan_operation/0]).
-export_type([rational/0]).
-export_type([rounding_method/0]).
-export_type([cash_flow_plan/0]).
-export_type([account_mapping/0]).
-export_type([constant_mapping/0]).
-export_type([final_posting/0]).
-export_type([final_cash_flow/0]).

%% Internal types
-type cash() :: ff_transaction:body().
-type account() :: ff_account:account().
-type domain_cash_flow_plan() :: dmsl_domain_thrift:'CashFlow'().

%% API
-spec gather_used_accounts(final_cash_flow()) -> [account()].
gather_used_accounts(#{postings := Postings}) ->
    lists:usort(lists:flatten([
        [S, D] || #{source := #{account := S}, destination := #{account := D}} <- Postings
    ])).

-spec finalize(cash_flow_plan(), account_mapping(), constant_mapping()) ->
    {ok, final_cash_flow()} | {error, _Error}.
finalize(_Plan, _Accounts, _Constants) ->
    % TODO: STUB
    {ok, #{postings => []}}.

-spec decode_plan(domain_cash_flow_plan()) ->
    {ok, cash_flow_plan()} | {error, _Error}.
decode_plan(_DomainPlan) ->
    % TODO: STUB
    {ok, #{postings => []}}.
