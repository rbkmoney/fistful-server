-module(ff_cash_flow).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-export([make_empty_final/0]).
-export([gather_used_accounts/1]).
-export([finalize/3]).
-export([add_fee/2]).
-export([combine/2]).
-export([inverse/1]).
-export([decode_domain_postings/1]).
-export([decode_domain_plan_volume/1]).
-export([compute_volume/2]).

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
    operation_amount |
    surplus.
-type plan_operation() ::
    {min_of, [plan_volume()]} |
    {max_of, [plan_volume()]}.

-type rational() :: genlib_rational:t().
-type rounding_method() ::
    default |
    round_half_towards_zero |  % https://en.wikipedia.org/wiki/Rounding#Round_half_towards_zero
    round_half_away_from_zero. % https://en.wikipedia.org/wiki/Rounding#Round_half_away_from_zero

-type cash_flow_plan() :: #{
    postings := [plan_posting()]
}.
-type cash_flow_fee() :: #{
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
    {system, subagent} |
    {provider, settlement}.
-type final_account() :: #{
    account := account(),
    type => plan_account()
}.

-type volume_finalize_error() ::
    {not_mapped_constant, plan_constant(), constant_mapping()} |
    {incomparable, {currency_mismatch, {cash(), cash()}}} |
    {operation_failed, {empty_list, plan_operation()}}.

-export_type([plan_posting/0]).
-export_type([plan_volume/0]).
-export_type([plan_constant/0]).
-export_type([plan_operation/0]).
-export_type([rational/0]).
-export_type([rounding_method/0]).
-export_type([cash_flow_plan/0]).
-export_type([cash_flow_fee/0]).
-export_type([account_mapping/0]).
-export_type([constant_mapping/0]).
-export_type([final_posting/0]).
-export_type([final_cash_flow/0]).
-export_type([plan_account/0]).

-export_type([volume_finalize_error/0]).
%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Internal types
-type cash() :: ff_transaction:body().
-type account() :: ff_account:account().

-type finalize_error() :: {postings, posting_finalize_error()}.
-type posting_finalize_error() ::
    account_finalize_error() |
    volume_finalize_error().
-type account_finalize_error() ::
    {not_mapped_plan_account, plan_account(), account_mapping()}.

%% API

-spec make_empty_final() -> final_cash_flow().
make_empty_final() ->
    #{postings => []}.

-spec gather_used_accounts(final_cash_flow()) -> [account()].
gather_used_accounts(#{postings := Postings}) ->
    lists:usort(lists:flatten([
        [S, D] || #{sender := #{account := S}, receiver := #{account := D}} <- Postings
    ])).

-spec finalize(cash_flow_plan(), account_mapping(), constant_mapping()) ->
    {ok, final_cash_flow()} | {error, finalize_error()}.
finalize(Plan, Accounts, Constants) ->
    do(fun () ->
        Postings = unwrap(postings, compute_postings(Plan, Accounts, Constants)),
        #{postings => Postings}
    end).

-spec add_fee(cash_flow_plan(), cash_flow_fee()) ->
    {ok, cash_flow_plan()}.
add_fee(#{postings := PlanPostings} = Plan, #{postings := FeePostings}) ->
    {ok, Plan#{postings => PlanPostings ++ FeePostings}}.

-spec combine(final_cash_flow(), final_cash_flow()) ->
    {ok, final_cash_flow()}.
combine(#{postings := Postings1} = Flow, #{postings := Postings2}) ->
    {ok, Flow#{postings => Postings1 ++ Postings2}}.

-spec inverse(final_cash_flow()) -> final_cash_flow().
inverse(#{postings := Postings} = Flow) ->
    Flow#{postings := lists:map(fun inverse_posting/1, Postings)}.

%% Domain cash flow unmarshalling

-spec decode_domain_postings(dmsl_domain_thrift:'CashFlow'()) ->
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
    ff_cash_flow:plan_account().
decode_domain_plan_account({_AccountNS, _AccountType} = Account) ->
    Account.

-spec decode_domain_plan_volume(dmsl_domain_thrift:'CashVolume'()) ->
    ff_cash_flow:plan_volume().
decode_domain_plan_volume({fixed, #domain_CashVolumeFixed{cash = Cash}}) ->
    {fixed, ff_dmsl_codec:unmarshal(cash, Cash)};
decode_domain_plan_volume({share, Share}) ->
    #domain_CashVolumeShare{
        parts = Parts,
        'of' = Of,
        rounding_method = RoundingMethod
    } = Share,
    {share, {decode_rational(Parts), Of, decode_rounding_method(RoundingMethod)}};
decode_domain_plan_volume({product, {Fun, CVs}}) ->
    {product, {Fun, lists:map(fun decode_domain_plan_volume/1, CVs)}}.

-spec decode_rounding_method(dmsl_domain_thrift:'RoundingMethod'() | undefined) ->
    ff_cash_flow:rounding_method().
decode_rounding_method(undefined) ->
    default;
decode_rounding_method(RoundingMethod) ->
    RoundingMethod.

-spec decode_rational(dmsl_base_thrift:'Rational'()) ->
    genlib_rational:t().
decode_rational(#'Rational'{p = P, q = Q}) ->
    genlib_rational:new(P, Q).

-spec compute_volume(plan_volume(), constant_mapping()) ->
    {ok, cash()} | {error, volume_finalize_error()}.
compute_volume({fixed, Cash}, _Constants) ->
    {ok, Cash};
compute_volume({share, {Rational, Constant, RoundingMethod}}, Constants) ->
    do(fun () ->
        {Amount, Currency} = unwrap(get_constant_value(Constant, Constants)),
        ResultAmount = genlib_rational:round(
            genlib_rational:mul(
                genlib_rational:new(Amount),
                Rational
            ),
            get_genlib_rounding_method(RoundingMethod)
        ),
        {ResultAmount, Currency}
    end);
compute_volume({product, {Operation, PlanVolumes}}, Constants) ->
    do(fun () ->
        Volumes = unwrap(compute_volumes(PlanVolumes, Constants)),
        unwrap(foldl_cash(Operation, Volumes))
    end).

-spec compute_volumes([plan_volume()], constant_mapping()) ->
    {ok, [cash()]} | {error, volume_finalize_error()}.
compute_volumes(Volumes, Constants) ->
    do(fun () ->
        [unwrap(compute_volume(V, Constants)) || V <- Volumes]
    end).

%% Internals

%% Inversing

-spec inverse_posting
    (plan_posting()) -> plan_posting();
    (final_posting()) -> final_posting().
inverse_posting(Posting) ->
    #{
        sender := Sender,
        receiver := Receiver
    } = Posting,
    Posting#{sender := Receiver, receiver := Sender}.

%% Finalizing

-spec compute_postings(cash_flow_plan(), account_mapping(), constant_mapping()) ->
    {ok, [final_posting()]} | {error, posting_finalize_error()}.
compute_postings(#{postings := PlanPostings}, Accounts, Constants) ->
    do(fun () ->
        [
            unwrap(construct_final_posting(PlanPosting, Accounts, Constants))
            || PlanPosting <- PlanPostings
        ]
    end).

-spec construct_final_posting(plan_posting(), account_mapping(), constant_mapping()) ->
    {ok, final_posting()} | {error, posting_finalize_error()}.
construct_final_posting(PlanPosting, Accounts, Constants) ->
    #{
        sender := PlanSender,
        receiver := PlanReceiver,
        volume := PlanVolume
    } = PlanPosting,
    PlanDetails = genlib_map:get(details, PlanPosting),
    do(fun () ->
        genlib_map:compact(#{
            sender => unwrap(construct_final_account(PlanSender, Accounts)),
            receiver => unwrap(construct_final_account(PlanReceiver, Accounts)),
            volume => unwrap(compute_volume(PlanVolume, Constants)),
            details => PlanDetails
        })
    end).

-spec construct_final_account(plan_account(), account_mapping()) ->
    {ok, final_account()} | {error, account_finalize_error()}.
construct_final_account(PlanAccount, Accounts) ->
    case maps:find(PlanAccount, Accounts) of
        {ok, Account} ->
            {ok, #{
                type => PlanAccount,
                account => Account
            }};
        error ->
            {error, {not_mapped_plan_account, PlanAccount, Accounts}}
    end.

-spec get_constant_value(plan_constant(), constant_mapping()) ->
    {ok, cash()} | {error, {not_mapped_constant, plan_constant(), constant_mapping()}}.
get_constant_value(Constant, Constants) ->
    case maps:find(Constant, Constants) of
        {ok, _Value} = Result ->
            Result;
        error ->
            {error, {not_mapped_constant, Constant, Constants}}
    end.

-spec get_genlib_rounding_method(rounding_method()) -> genlib_rational:rounding_method().
get_genlib_rounding_method(default) ->
    round_half_away_from_zero;
get_genlib_rounding_method(round_half_towards_zero) ->
    round_half_towards_zero;
get_genlib_rounding_method(round_half_away_from_zero) ->
    round_half_away_from_zero.

foldl_cash(Operation, []) ->
    {error, {operation_failed, {empty_list, {Operation, []}}}};
foldl_cash(min_of, [Cash| CTail]) ->
    do_foldl(fun cash_min/2, Cash, CTail);
foldl_cash(max_of, [Cash| CTail]) ->
    do_foldl(fun cash_max/2, Cash, CTail).

-spec do_foldl(Fun, Acc, [T]) -> {ok, Acc} | {error, Reason} when
    Fun :: fun((T, Acc) -> {ok, Acc} | {error, Reason}).
do_foldl(Fun, Acc0, List) ->
    do(fun() ->
        lists:foldl(fun(H, Acc) -> unwrap(Fun(H, Acc)) end, Acc0, List)
    end).

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
