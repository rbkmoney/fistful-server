-module(ff_cash_flow_codec).

-behaviour(ff_codec).

-include_lib("fistful_proto/include/ff_proto_cashflow_thrift.hrl").

-export([marshal/2]).
-export([unmarshal/2]).

%% API

-spec marshal(ff_codec:type_name(), ff_codec:decoded_value()) ->
    ff_codec:encoded_value().

marshal({list, T}, V) ->
    [marshal(T, E) || E <- V];

marshal(final_cash_flow, #{postings := Postings}) ->
    #cashflow_FinalCashFlow{
        postings = marshal({list, postings}, Postings)
    };
marshal(postings, Posting) ->
    #{
        sender := Sender,
        receiver := Receiver,
        volume := Cash
    } = Posting,
    Details = maps:get(details, Posting, undefined),
    #cashflow_FinalCashFlowPosting{
        source      = marshal(final_cash_flow_account, Sender),
        destination = marshal(final_cash_flow_account, Receiver),
        volume      = marshal(cash, Cash),
        details     = marshal(string, Details)
    };
marshal(final_cash_flow_account, #{
    account := Account,
    type := AccountType
}) ->
    #{id := AccountID} = Account,
    #cashflow_FinalCashFlowAccount{
        account_type   = marshal(account_type, AccountType),
        account_id     = marshal(id, AccountID), % for compatability, deprecate
        account        = ff_codec:marshal(account, Account)
    };

marshal(account_type, CashflowAccount) ->
    % Mapped to thrift type WalletCashFlowAccount as is
    CashflowAccount;

marshal(T, V) ->
    ff_codec:marshal(T, V).


-spec unmarshal(ff_codec:type_name(), ff_codec:encoded_value()) ->
    ff_codec:decoded_value().

unmarshal({list, T}, V) ->
    [unmarshal(T, E) || E <- V];

unmarshal(final_cash_flow, #cashflow_FinalCashFlow{
    postings = Postings
}) ->
    #{
        postings => unmarshal({list, postings}, Postings)
    };
unmarshal(postings, #cashflow_FinalCashFlowPosting{
    source = Source,
    destination = Destination,
    volume = Cash,
    details = Details
}) ->
    genlib_map:compact(#{
        sender      => unmarshal(final_cash_flow_account, Source),
        receiver    => unmarshal(final_cash_flow_account, Destination),
        volume      => unmarshal(cash, Cash),
        details     => maybe_unmarshal(string, Details)
    });
unmarshal(final_cash_flow_account, #cashflow_FinalCashFlowAccount{
    account_type = AccountType,
    account      = Account
}) ->
    #{
        account => ff_codec:unmarshal(account, Account),
        type    => unmarshal(account_type, AccountType)
    };

unmarshal(account_type, CashflowAccount) ->
    % Mapped to thrift type WalletCashFlowAccount as is
    CashflowAccount;

unmarshal(T, V) ->
    ff_codec:unmarshal(T, V).

%% Internals

maybe_unmarshal(_Type, undefined) ->
    undefined;
maybe_unmarshal(Type, Value) ->
    unmarshal(Type, Value).

%% Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> _.

-spec final_cash_flow_symmetry_test() -> _.
final_cash_flow_symmetry_test() ->
    PostingFn = fun() ->
        #{
            sender => #{
                account => #{
                    id => genlib:unique(),
                    identity => genlib:unique(),
                    currency => <<"RUB">>,
                    accounter_account_id => 123
                },
                type => sender_source
            },
            receiver => #{
                account => #{
                    id => genlib:unique(),
                    identity => genlib:unique(),
                    currency => <<"USD">>,
                    accounter_account_id => 321
                },
                type => receiver_settlement
            },
            volume => {100, <<"EUR">>}
        }
    end,
    CashFlow = #{
        postings => [
            PostingFn(),
            PostingFn()
        ]
    },
    ?assertEqual(CashFlow, unmarshal(final_cash_flow, marshal(final_cash_flow, CashFlow))).

-endif.
