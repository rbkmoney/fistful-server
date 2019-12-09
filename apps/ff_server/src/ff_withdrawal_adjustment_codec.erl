-module(ff_withdrawal_adjustment_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_withdrawal_adjustment_thrift.hrl").

-export([marshal/2]).
-export([unmarshal/2]).

%% API

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) ->
    ff_codec:encoded_value().

marshal(change, {created, Adjustment}) ->
    {created, #wthd_adj_CreatedChange{adjustment = marshal(adjustment, Adjustment)}};
marshal(change, {status_changed, Status}) ->
    {status_changed, #wthd_adj_StatusChange{status = marshal(status, Status)}};
marshal(change, {p_transfer, TransferChange}) ->
    {transfer, #wthd_adj_TransferChange{payload = ff_p_transfer_codec:marshal(change, TransferChange)}};

marshal(adjustment, Adjustment) ->
    #wthd_adj_Adjustment{
        id = marshal(id, ff_adjustment:id(Adjustment)),
        status = maybe_marshal(status, ff_adjustment:status(Adjustment)),
        changes_plan = marshal(changes_plan, ff_adjustment:changes_plan(Adjustment)),
        created_at = marshal(timestamp_ms, ff_adjustment:created_at(Adjustment)),
        domain_revision = marshal(domain_revision, ff_adjustment:domain_revision(Adjustment)),
        party_revision = marshal(domain_revision, ff_adjustment:party_revision(Adjustment)),
        external_id = maybe_marshal(id, ff_adjustment:external_id(Adjustment))
    };
marshal(adjustment_params, Params) ->
    #wthd_adj_AdjustmentParams{
        id = marshal(id, maps:get(id, Params)),
        change = marshal(change_request, maps:get(change, Params)),
        external_id = maybe_marshal(id, maps:get(external_id, Params, undefined))
    };
marshal(adjustment_state, Adjustment) ->
    #wthd_adj_AdjustmentState{
        adjustment = marshal(adjustment, Adjustment)
    };

marshal(status, pending) ->
    {pending, #wthd_adj_Pending{}};
marshal(status, succeeded) ->
    {succeeded, #wthd_adj_Succeeded{}};

marshal(changes_plan, Plan) ->
    #wthd_adj_ChangesPlan{
        new_cash_flow = maybe_marshal(cash_flow_change_plan, maps:get(new_cash_flow, Plan, undefined)),
        new_status = maybe_marshal(status_change_plan, maps:get(new_status, Plan, undefined))
    };
marshal(cash_flow_change_plan, Plan) ->
    OldCashFLow = ff_cash_flow_codec:marshal(final_cash_flow, maps:get(old_cash_flow_inverted, Plan)),
    NewCashFlow = ff_cash_flow_codec:marshal(final_cash_flow, maps:get(new_cash_flow, Plan)),
    #wthd_adj_CashFlowChangePlan{
        old_cash_flow_inverted = OldCashFLow,
        new_cash_flow = NewCashFlow
    };
marshal(status_change_plan, Plan) ->
    #wthd_adj_StatusChangePlan{
        new_status = ff_withdrawal_status_codec:marshal(status, maps:get(new_status, Plan))
    };

marshal(change_request, {change_status, Status}) ->
    {change_status, #wthd_adj_ChangeStatusRequest{
        new_status = ff_withdrawal_status_codec:marshal(status, Status)
    }};

marshal(T, V) ->
    ff_codec:marshal(T, V).


-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) ->
    ff_codec:decoded_value().

unmarshal(change, {created, #wthd_adj_CreatedChange{adjustment = Adjustment}}) ->
    {created, unmarshal(adjustment, Adjustment)};
unmarshal(change, {status_changed, #wthd_adj_StatusChange{status = Status}}) ->
    {status_changed, unmarshal(status, Status)};
unmarshal(change, {transfer, #wthd_adj_TransferChange{payload = TransferChange}}) ->
    {p_transfer, ff_p_transfer_codec:unmarshal(change, TransferChange)};

unmarshal(adjustment, Adjustment) ->
    #{
        id => unmarshal(id, Adjustment#wthd_adj_Adjustment.id),
        status => unmarshal(status, Adjustment#wthd_adj_Adjustment.status),
        changes_plan => unmarshal(changes_plan, Adjustment#wthd_adj_Adjustment.changes_plan),
        created_at => unmarshal(timestamp_ms, Adjustment#wthd_adj_Adjustment.created_at),
        domain_revision => unmarshal(domain_revision, Adjustment#wthd_adj_Adjustment.domain_revision),
        party_revision => unmarshal(domain_revision, Adjustment#wthd_adj_Adjustment.party_revision),
        external_id => maybe_unmarshal(id, Adjustment#wthd_adj_Adjustment.external_id)
    };

unmarshal(adjustment_params, Params) ->
    genlib_map:compact(#{
        id => unmarshal(id, Params#wthd_adj_AdjustmentParams.id),
        change => unmarshal(change_request, Params#wthd_adj_AdjustmentParams.change),
        external_id => maybe_unmarshal(id, Params#wthd_adj_AdjustmentParams.external_id)
    });

unmarshal(status, {pending, #wthd_adj_Pending{}}) ->
    pending;
unmarshal(status, {succeeded, #wthd_adj_Succeeded{}}) ->
    succeeded;

unmarshal(changes_plan, Plan) ->
    genlib_map:compact(#{
        new_cash_flow => maybe_unmarshal(cash_flow_change_plan, Plan#wthd_adj_ChangesPlan.new_cash_flow),
        new_status => maybe_unmarshal(status_change_plan, Plan#wthd_adj_ChangesPlan.new_status)
    });
unmarshal(cash_flow_change_plan, Plan) ->
    OldCashFlow = Plan#wthd_adj_CashFlowChangePlan.old_cash_flow_inverted,
    NewCashFlow = Plan#wthd_adj_CashFlowChangePlan.new_cash_flow,
    #{
        old_cash_flow_inverted => ff_cash_flow_codec:unmarshal(final_cash_flow, OldCashFlow),
        new_cash_flow => ff_cash_flow_codec:unmarshal(final_cash_flow, NewCashFlow)
    };
unmarshal(status_change_plan, Plan) ->
    Status = Plan#wthd_adj_StatusChangePlan.new_status,
    #{
        new_status => ff_withdrawal_status_codec:unmarshal(status, Status)
    };

unmarshal(change_request, {change_status, Request}) ->
    Status = Request#wthd_adj_ChangeStatusRequest.new_status,
    {change_status, ff_withdrawal_status_codec:unmarshal(status, Status)};

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

%% Internals

maybe_unmarshal(_Type, undefined) ->
    undefined;
maybe_unmarshal(Type, Value) ->
    unmarshal(Type, Value).

maybe_marshal(_Type, undefined) ->
    undefined;
maybe_marshal(Type, Value) ->
    marshal(Type, Value).