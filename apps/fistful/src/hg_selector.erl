%%% Domain selectors manipulation
%%%
%%% TODO
%%%  - Manipulating predicates w/o respect to their struct infos is dangerous
%%%  - Decide on semantics
%%%     - First satisfiable predicate wins?
%%%       If not, it would be harder to join / overlay selectors

-module(hg_selector).

%%

-type t() ::
    dmsl_domain_thrift:'CurrencySelector'() |
    dmsl_domain_thrift:'CategorySelector'() |
    dmsl_domain_thrift:'CashLimitSelector'() |
    dmsl_domain_thrift:'CashFlowSelector'() |
    dmsl_domain_thrift:'PaymentMethodSelector'() |
    dmsl_domain_thrift:'ProviderSelector'() |
    dmsl_domain_thrift:'TerminalSelector'() |
    dmsl_domain_thrift:'SystemAccountSetSelector'() |
    dmsl_domain_thrift:'ExternalAccountSetSelector'() |
    dmsl_domain_thrift:'HoldLifetimeSelector'() |
    dmsl_domain_thrift:'CashValueSelector'() |
    dmsl_domain_thrift:'CumulativeLimitSelector'() |
    dmsl_domain_thrift:'WithdrawalProviderSelector'() |
    dmsl_domain_thrift:'P2PProviderSelector'() |
    dmsl_domain_thrift:'P2PInspectorSelector'() |
    dmsl_domain_thrift:'TimeSpanSelector'() |
    dmsl_domain_thrift:'FeeSelector'().

-type value() ::
    _. %% FIXME

-type varset() :: #{
    category        => dmsl_domain_thrift:'CategoryRef'(),
    currency        => dmsl_domain_thrift:'CurrencyRef'(),
    cost            => dmsl_domain_thrift:'Cash'(),
    payment_tool    => dmsl_domain_thrift:'PaymentTool'(),
    party_id        => dmsl_domain_thrift:'PartyID'(),
    shop_id         => dmsl_domain_thrift:'ShopID'(),
    risk_score      => dmsl_domain_thrift:'RiskScore'(),
    flow            => instant | {hold, dmsl_domain_thrift:'HoldLifetime'()},
    payout_method   => dmsl_domain_thrift:'PayoutMethodRef'(),
    wallet_id       => dmsl_domain_thrift:'WalletID'(),
    identification_level => dmsl_domain_thrift:'ContractorIdentificationLevel'(),
    p2p_tool        => dmsl_domain_thrift:'P2PTool'()
}.

-export_type([varset/0]).

-export([fold/3]).
-export([collect/1]).
-export([reduce/2]).
-export([reduce_to_value/2]).

-define(const(Bool), {constant, Bool}).

%%

-spec fold(FoldWith :: fun((Value :: _, Acc) -> Acc), Acc, t()) ->
    Acc when
        Acc :: term().

fold(FoldWith, Acc, {value, V}) ->
    FoldWith(V, Acc);
fold(FoldWith, Acc, {decisions, Ps}) ->
    fold_decisions(FoldWith, Acc, Ps).

fold_decisions(FoldWith, Acc, [{_Type, _, S} | Rest]) ->
    fold_decisions(FoldWith, fold(FoldWith, Acc, S), Rest);
fold_decisions(_, Acc, []) ->
    Acc.

-spec collect(t()) ->
    [value()].

collect(S) ->
    fold(fun (V, Acc) -> [V | Acc] end, [], S).


-spec reduce_to_value(t(), varset()) -> {ok, value()} | {error, term()}.

reduce_to_value(Selector, VS) ->
    case reduce(Selector, VS) of
        {value, Value} ->
            {ok, Value};
        _ ->
            {error, {misconfiguration, {'Can\'t reduce selector to value', Selector, VS}}}
    end.

-spec reduce(t(), varset()) ->
    t().

reduce({value, _} = V, _) ->
    V;
reduce({decisions, Ps}, VS) ->
    case reduce_decisions(Ps, VS) of
        [{_Type, ?const(true), S} | _] ->
            S;
        Ps1 ->
            {decisions, Ps1}
    end.

reduce_decisions([{Type, V, S} | Rest], VS) ->
    case reduce_predicate(V, VS) of
        ?const(false) ->
            reduce_decisions(Rest, VS);
        V1 ->
            case reduce(S, VS) of
                {decisions, []} ->
                    reduce_decisions(Rest, VS);
                S1 ->
                    [{Type, V1, S1} | reduce_decisions(Rest, VS)]
            end
    end;
reduce_decisions([], _) ->
    [].

reduce_predicate(?const(B), _) ->
    ?const(B);

reduce_predicate({condition, C0}, VS) ->
    case reduce_condition(C0, VS) of
        ?const(B) ->
            ?const(B);
        C1 ->
            {condition, C1}
    end;

reduce_predicate({is_not, P0}, VS) ->
    case reduce_predicate(P0, VS) of
        ?const(B) ->
            ?const(not B);
        P1 ->
            {is_not, P1}
    end;

reduce_predicate({all_of, Ps}, VS) ->
    reduce_combination(all_of, false, Ps, VS, []);

reduce_predicate({any_of, Ps}, VS) ->
    reduce_combination(any_of, true, Ps, VS, []).

reduce_combination(Type, Fix, [P | Ps], VS, PAcc) ->
    case reduce_predicate(P, VS) of
        ?const(Fix) ->
            ?const(Fix);
        ?const(_) ->
            reduce_combination(Type, Fix, Ps, VS, PAcc);
        P1 ->
            reduce_combination(Type, Fix, Ps, VS, [P1 | PAcc])
    end;
reduce_combination(_, Fix, [], _, []) ->
    ?const(not Fix);
reduce_combination(Type, _, [], _, PAcc) ->
    {Type, lists:reverse(PAcc)}.

reduce_condition(C, VS) ->
    case hg_condition:test(C, VS) of
        B when is_boolean(B) ->
            ?const(B);
        undefined ->
            % Irreducible, return as is
            C
    end.
