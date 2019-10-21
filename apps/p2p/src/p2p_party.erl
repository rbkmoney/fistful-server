-module(p2p_party).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-type terms()               :: ff_party:terms().
-type constant_mapping()    :: ff_cash_flow:constant_mapping().
-type cash_flow_plan()      :: ff_cash_flow:cash_flow_plan().
-type plan_volume()         :: ff_cash_flow:plan_volume().
-type cash()                :: ff_cash:cash().
-type volume_finalize_error() :: ff_cash_flow:volume_finalize_error().
-export([get_cash_flow_plan/1]).
-export([compute_volume/2]).

-spec get_cash_flow_plan(terms()) ->
    {ok, cash_flow_plan()} | {error, _Error}.
get_cash_flow_plan(Terms) ->
    #domain_TermSet{
        wallets = #domain_WalletServiceTerms{
            p2p = #domain_P2PServiceTerms{
                cash_flow = CashFlow
            }
        }
    } = Terms,
    {value, DomainPostings} = CashFlow,
    Postings = ff_cash_flow:decode_domain_postings(DomainPostings),
    {ok, #{postings => Postings}}.

-spec compute_volume(plan_volume(), constant_mapping()) ->
    {ok, cash()} | {error, volume_finalize_error()}.
compute_volume(PlanVolume, Constant) ->
    ff_cash_flow:compute_volume(PlanVolume, Constant).
