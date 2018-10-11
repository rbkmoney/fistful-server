-module(ff_cash_flow).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

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
    plan_account() => account()
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

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Internal types
-type cash() :: ff_transaction:body().
-type account() :: ff_account:account().
-type domain_plan_postings() :: dmsl_domain_thrift:'CashFlow'().
-type genlib_rounding_method() :: round_half_towards_zero | round_half_away_from_zero.

-type finalize_error() :: {postings, posting_finalize_error()}.
-type posting_finalize_error() ::
    account_finalize_error() |
    volume_finalize_error().
-type account_finalize_error() ::
    {not_mapped_plan_account, plan_account(), account_mapping()}.
-type volume_finalize_error() ::
    {not_mapped_constant, plan_constant(), constant_mapping()} |
    {incomparable, {currency_mismatch, {cash(), cash()}}}.

%% API
-spec gather_used_accounts(final_cash_flow()) -> [account()].
gather_used_accounts(#{postings := Postings}) ->
    lists:usort(lists:flatten([
        [S, D] || #{source := #{account := S}, destination := #{account := D}} <- Postings
    ])).

-spec finalize(cash_flow_plan(), account_mapping(), constant_mapping()) ->
    {ok, final_cash_flow()} | {error, finalize_error()}.
finalize(Plan, Accounts, Constants) ->
    do(fun () ->
        Postings = unwrap(postings, compute_postings(Plan, Accounts, Constants)),
        #{postings => Postings}
    end).

-spec decode_plan(domain_plan_postings()) ->
    {ok, cash_flow_plan()} | {error, _Error}.
decode_plan(DomainPostings) ->
    Postings = decode_domain_postings(DomainPostings),
    {ok, #{postings => Postings}}.

%% Internals

%% Finalizing

-spec compute_postings([plan_posting()], account_mapping(), constant_mapping()) ->
    {ok, [final_posting()]} | {error, posting_finalize_error()}.
compute_postings(Plan, Accounts, Constants) ->
    do(fun () ->
        [
            unwrap(construct_final_posting(PlanPosting, Accounts, Constants))
            || PlanPosting <- Plan
        ]
    end).

-spec construct_final_posting(plan_posting(), account_mapping(), constant_mapping()) ->
    {ok, final_posting()} | {error, posting_finalize_error()}.
construct_final_posting(PlanPosting, Accounts, Constants) ->
    #{
        sender := PlanSender,
        receiver := PlanReceiver,
        volume := PlanVolume,
        details := PlanDetails
    } = PlanPosting,
    do(fun () ->
        #{
            sender => unwrap(construct_final_account(PlanSender, Accounts)),
            receiver => unwrap(construct_final_account(PlanReceiver, Accounts)),
            volume => unwrap(compute_volume(PlanVolume, Constants)),
            details => PlanDetails 
        }
    end).

-spec construct_final_account(plan_account(), account_mapping()) ->
    {ok, final_account()} | {error, account_finalize_error()}.
construct_final_account(PlanAccount, Accounts) ->
    case maps:is_key(PlanAccount, Accounts) of
        true ->
            {ok, #{
                type => PlanAccount,
                account => maps:get(PlanAccount, Accounts)
            }};
        false ->
            {error, {not_mapped_plan_account, PlanAccount, Accounts}}
    end.

-spec compute_volume(plan_volume(), constant_mapping()) ->
    {ok, cash()} | {error, volume_finalize_error()}.
compute_volume({fixed, Cash}, _Constants) ->
    {ok, Cash};
compute_volume({share, {Rational, Constant, RoundingMethod}}, Constants) ->
    do(fun () ->
        {Amount, Currency} = unwrap(get_constant_value(Constant, Constants)),
        {P, Q} = Rational,
        ResultAmount = genlib_rational:round(
            genlib_rational:mul(
                genlib_rational:new(Amount),
                genlib_rational:new(P, Q)
            ),
            get_genlib_rounding_method(RoundingMethod)
        ),
        {ResultAmount, Currency}
    end);
compute_volume({product, {min_of, Volumes}}, Constants) ->
    do(fun () ->
        [Cash| CTail] = unwrap(compute_volumes(Volumes, Constants)),
        unwrap(do_foldl(fun cash_min/2, Cash, CTail))
    end);
compute_volume({product, {max_of, Volumes}}, Constants) ->
    do(fun () ->
        [Cash| CTail] = unwrap(compute_volumes(Volumes, Constants)),
        unwrap(do_foldl(fun cash_max/2, Cash, CTail))
    end).

-spec compute_volumes([plan_volume()], constant_mapping()) ->
    {ok, [cash()]} | {error, volume_finalize_error()}.
compute_volumes(Volumes, Constants) ->
    do(fun () ->
        [unwrap(compute_volume(V, Constants)) || V <- Volumes]
    end).

-spec get_constant_value(plan_constant(), constant_mapping()) ->
    {ok, cash()} | {error, {not_mapped_constant, plan_constant(), constant_mapping()}}.
get_constant_value(Constant, Constants) ->
    case maps:is_key(Constant, Constants) of
        true ->
            {ok, maps:get(Constant, Constants)};
        false ->
            {error, {not_mapped_constant, Constant, Constants}}
    end.

-spec get_genlib_rounding_method(rounding_method()) -> genlib_rounding_method().
get_genlib_rounding_method(default) ->
    round_half_away_from_zero;
get_genlib_rounding_method(round_half_towards_zero) ->
    round_half_towards_zero;
get_genlib_rounding_method(round_half_away_from_zero) ->
    round_half_away_from_zero.

-spec do_foldl(Fun, Acc, [T]) -> {ok, Acc} | {error, Reason} when
    Fun :: fun((T, Acc) -> {ok, Acc} | {error, Reason}).
do_foldl(_Fun, Acc, []) ->
    {ok, Acc};
do_foldl(Fun, Acc0, [H | Tail]) ->
    case Fun(H, Acc0) of
        {ok, Acc1} ->
            do_foldl(Fun, Acc1, Tail);
        {error, _Reason} = Error ->
            Error
    end.

-spec cash_min(cash(), cash()) ->
    {ok, cash()} | {error, {incomparable, {currency_mismatch, {cash(), cash()}}}}.
cash_min({Amount1, Currency1}, {Amount2, Currency2}) when Currency1 =:= Currency2 ->
    {ok, {erlang:min(Amount1, Amount2), Currency1}};
cash_min({_Amount1, Currency1} = Cash1, {_Amount2, Currency2} = Cash2) when Currency1 =/= Currency2 ->
    {error, {incomparable, {currency_mismatch, {Cash1, Cash2}}}}.

-spec cash_max(cash(), cash()) ->
    {ok, cash()} | {error, {incomparable, {currency_mismatch, {cash(), cash()}}}}.
cash_max({Amount1, Currency1}, {Amount2, Currency2}) when Currency1 =:= Currency2 ->
    {ok, {erlang:max(Amount1, Amount2), Currency1}};
cash_max({_Amount1, Currency1} = Cash1, {_Amount2, Currency2} = Cash2) when Currency1 =/= Currency2 ->
    {error, {incomparable, {currency_mismatch, {Cash1, Cash2}}}}.

%% Domain cash flow unmarshalling

-spec decode_domain_postings(domain_plan_postings()) ->
    [plan_posting()].
decode_domain_postings(DomainPostings) ->
    [decode_domain_posting(P) || P <- DomainPostings].

-spec decode_domain_posting(dmsl_domain_thrift:'CashFlowPosting'()) ->
    plan_posting().
decode_domain_posting(
    #domain_CashFlowPosting{
        source = Source,
        destination = Destination,
        volume = Volume,
        details = Details
    }
) ->
    #{
        sender => decode_domain_plan_account(Source),
        receiver => decode_domain_plan_account(Destination),
        volume => decode_domain_plan_volume(Volume),
        details => Details
    }.

-spec decode_domain_plan_account(dmsl_domain_thrift:'CashFlowAccount'()) ->
    plan_account().
decode_domain_plan_account({_AccountNS, _AccountType} = Account) ->
    Account.

-spec decode_domain_plan_volume(dmsl_domain_thrift:'CashVolume'()) ->
    plan_volume().
decode_domain_plan_volume({fixed, #domain_CashVolumeFixed{cash = Cash}}) ->
    {fixed, decode_domain_cash(Cash)};
decode_domain_plan_volume({share, Share}) ->
    #domain_CashVolumeShare{
        parts = #'Rational'{p = P, q = Q},
        'of' = Of,
        rounding_method = RoundingMethod
    } = Share,
    {share, {{P, Q}, Of, RoundingMethod}};
decode_domain_plan_volume({product, {Fun, CVs}}) ->
    {product, {Fun, lists:map(fun decode_domain_plan_volume/1, CVs)}}.

-spec decode_domain_cash(dmsl_domain_thrift:'Cash'()) ->
    cash().
decode_domain_cash(
    #domain_Cash{
        amount = Amount,
        currency = #domain_CurrencyRef{
            symbolic_code = SymbolicCode
        }
    }
) ->
    {Amount, SymbolicCode}.
