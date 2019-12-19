-module(ff_adjustment_codec).

-export([marshal/3]).
-export([unmarshal/2]).

-type prefix() :: #{
    transfer := binary(),
    adjustment := binary(),
    status := binary()
}.

%% Some hack

% -spec record_transfer(prefix(), binary()) ->
%     atom().

% record_transfer(#{transfer := Prefix}, Name) ->
%     erlang:binary_to_atom(<<Prefix/binary, "_", Name/binary>>, latin1).

-spec record_adjustment(prefix(), binary()) ->
    atom().

record_adjustment(#{adjustment := Prefix}, Name) ->
    erlang:binary_to_atom(<<Prefix/binary, "_", Name/binary>>, latin1).

-spec record_status(prefix(), binary()) ->
    atom().

record_status(#{status := Prefix}, Name) ->
    erlang:binary_to_atom(<<Prefix/binary, "_", Name/binary>>, latin1).

%% API

-spec marshal(prefix(), ff_codec:type_name(), ff_codec:decoded_value()) ->
    ff_codec:encoded_value().

marshal(Prefix, {list, T}, V) ->
    [marshal(Prefix, T, E) || E <- V];

marshal(Prefix, change, {created, Adjustment}) ->
    {created, {record_adjustment(Prefix, <<"CreatedChange">>), marshal(Prefix, adjustment, Adjustment)}};
marshal(Prefix, change, {p_transfer, TransferChange}) ->
    {transfer, {record_adjustment(Prefix, <<"TransferChange">>), ff_p_transfer_codec:marshal(event, TransferChange)}};
marshal(Prefix, change, {status_changed, Status}) ->
    {status_changed, {record_adjustment(Prefix, <<"StatusChange">>), marshal(Prefix, status, Status)}};

marshal(Prefix, adjustment, Adjustment = #{
    id := ID,
    status := Status,
    created_at := CreatedAt,
    changes_plan := ChangesPlan,
    party_revision := PartyRevision,
    domain_revision := DomainRevision,
    operation_timestamp := OperationTimestamp
}) ->
    ExternalID = maps:get(external_id, Adjustment, undefined),
    {
        record_adjustment(Prefix, <<"Adjustment">>),
        marshal(Prefix, id, ID),
        marshal(Prefix, status, Status),
        marshal(Prefix, changes_plan, ChangesPlan),
        marshal(Prefix, timestamp, ff_time:to_rfc3339(CreatedAt)),
        marshal(Prefix, integer, DomainRevision),
        marshal(Prefix, integer, PartyRevision),
        marshal(Prefix, timestamp, ff_time:to_rfc3339(OperationTimestamp)),
        maybe_marshal(Prefix, id, ExternalID)
    };

marshal(Prefix, status, pending) ->
    {pending, {record_adjustment(Prefix, <<"Pending">>)}};
marshal(Prefix, status, succeeded) ->
    {succeeded, {record_adjustment(Prefix, <<"Succeeded">>)}};

marshal(Prefix, changes_plan, ChangesPlan) ->
    NewCashFlow = maps:get(new_cash_flow, ChangesPlan, undefined),
    NewStatus = maps:get(new_status, ChangesPlan, undefined),
    {
        record_adjustment(Prefix, <<"ChangesPlan">>),
        maybe_marshal(Prefix, new_cash_flow, NewCashFlow),
        maybe_marshal(Prefix, status_change_plan, NewStatus)
    };

marshal(Prefix, new_cash_flow, #{
    old_cash_flow_inverted := OldCashFlowInverted,
    new_cash_flow := NewCashFlow
}) ->
    {
        record_adjustment(Prefix, <<"CashFlowChangePlan">>),
        ff_p_transfer_codec:marshal(final_cash_flow, OldCashFlowInverted),
        ff_p_transfer_codec:marshal(final_cash_flow, NewCashFlow)
    };

marshal(Prefix, status_change_plan, Status) ->
    {
        record_adjustment(Prefix, <<"StatusChangePlan">>),
        maybe_marshal(Prefix, target_status, Status)
    };

marshal(Prefix, target_status, pending) ->
    {pending, {record_status(Prefix, <<"Pending">>)}};
marshal(Prefix, target_status, succeeded) ->
    {succeeded, {record_status(Prefix, <<"Succeeded">>)}};
marshal(Prefix, target_status, {failed, Failure}) ->
    {failed, {record_status(Prefix, <<"Failed">>), marshal(Prefix, failure, Failure)}};

marshal(Prefix, timestamp, Timestamp) ->
    marshal(Prefix, string, Timestamp);

marshal(_Prefix, T, V) ->
    ff_codec:marshal(T, V).


-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) ->
    ff_codec:decoded_value().

unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];

unmarshal(change, {created, {_CreatedChange, Adjustment}}) ->
    {created, unmarshal(adjustment, Adjustment)};
unmarshal(change, {transfer, {_TransferChange, Change}}) ->
    {p_transfer, ff_p_transfer_codec:unmarshal(event, Change)};
unmarshal(change, {status_changed, {_StatusChange, Status}}) ->
    {status_changed, unmarshal(status, Status)};

unmarshal(adjustment, {
    _Adjustment,
    ID,
    Status,
    ChangesPlan,
    CreatedAt,
    DomainRevision,
    PartyRevision,
    OperationTimestamp,
    ExternalID
}) ->
    #{
        id => unmarshal(id, ID),
        status => unmarshal(status, Status),
        changes_plan => unmarshal(changes_plan, ChangesPlan),
        created_at => ff_time:from_rfc3339(unmarshal(timestamp, CreatedAt)),
        domain_revision => unmarshal(integer, DomainRevision),
        party_revision => unmarshal(integer, PartyRevision),
        operation_timestamp => ff_time:from_rfc3339(unmarshal(timestamp, OperationTimestamp)),
        external_id => maybe_unmarshal(id, ExternalID)
    };

unmarshal(status, {pending, _Pending}) ->
    pending;
unmarshal(status, {succeeded, _Succeeded}) ->
    succeeded;

unmarshal(changes_plan, {
    _ChangesPlan,
    NewCashFlow,
    NewStatus
}) ->
    genlib_map:compact(#{
        new_cash_flow => maybe_unmarshal(new_cash_flow, NewCashFlow),
        new_status => maybe_unmarshal(status_change_plan, NewStatus)
    });

unmarshal(new_cash_flow, {
    _CashFlowChangePlan,
    OldCashFlowInverted,
    NewCashFlow
}) ->
    #{
        old_cash_flow_inverted => ff_p_transfer_codec:unmarshal(final_cash_flow, OldCashFlowInverted),
        new_cash_flow => ff_p_transfer_codec:unmarshal(final_cash_flow, NewCashFlow)
    };

unmarshal(status_change_plan, {_StatusChangePlan, Status}) ->
    unmarshal(target_status, Status);

unmarshal(target_status, {pending, _Pending}) ->
    pending;
unmarshal(target_status, {succeeded, _Succeeded}) ->
    succeeded;
unmarshal(target_status, {failed, {_Failed, Failure}}) ->
    {failed, unmarshal(failure, Failure)};

unmarshal(timestamp, Timestamp) ->
    unmarshal(string, Timestamp);

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

%% Internals

maybe_marshal(_Prefix, _Type, undefined) ->
    undefined;
maybe_marshal(Prefix, Type, Value) ->
    marshal(Prefix, Type, Value).

maybe_unmarshal(_Type, undefined) ->
    undefined;
maybe_unmarshal(Type, Value) ->
    unmarshal(Type, Value).

%% TESTS

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> _.

-spec adjustment_codec_test() -> _.
adjustment_codec_test() ->
    Prefix = #{
        transfer => <<"transfer">>,
        adjustment => <<"adjustment">>,
        status => <<"status">>
    },

    FinalCashFlow = #{
        postings => []
    },

    CashFlowChange = #{
        old_cash_flow_inverted => FinalCashFlow,
        new_cash_flow => FinalCashFlow
    },

    Plan = #{
        new_cash_flow => CashFlowChange,
        new_status => succeeded
    },

    Adjustment = #{
        id => genlib:unique(),
        status => pending,
        changes_plan => Plan,
        created_at => ff_time:now(),
        domain_revision => 123,
        party_revision => 321,
        operation_timestamp => ff_time:now(),
        external_id => genlib:unique()
    },

    Transfer = #{
        final_cash_flow => FinalCashFlow
    },

    Changes = [
        {created, Adjustment},
        {p_transfer, {created, Transfer}},
        {status_changed, pending}
    ],
    ?assertEqual(Changes, unmarshal({list, change}, marshal(Prefix, {list, change}, Changes))).

-endif.