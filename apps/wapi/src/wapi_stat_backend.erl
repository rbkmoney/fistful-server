-module(wapi_stat_backend).

-include_lib("fistful_proto/include/ff_proto_fistful_stat_thrift.hrl").

-export([list_wallets/2]).
-export([list_withdrawals/2]).
-export([list_deposits/2]).

-type req_data() :: wapi_handler:req_data().
-type handler_context() :: wapi_handler:context().
-type response_data() :: wapi_handler:response_data().

-spec list_wallets(req_data(), handler_context()) ->
    {ok, response_data()} | {error, StatError}
    when StatError ::
        {invalid | bad_token, binary()}.

list_wallets(Params, Context) ->
    Dsl = create_dsl(wallets, Params, Context),
    Req = create_request(Dsl, maps:get(continuationToken, Params, undefined)),
    Result = wapi_handler_utils:service_call({fistful_stat, 'GetWallets', [Req]}, Context),
    process_result(Result).

-spec list_withdrawals(req_data(), handler_context()) ->
    {ok, response_data()} | {error, StatError}
    when StatError ::
        {invalid | bad_token, binary()}.

list_withdrawals(Params, Context) ->
    Dsl = create_dsl(withdrawals, Params, Context),
    Req = create_request(Dsl, maps:get(continuationToken, Params, undefined)),
    Result = wapi_handler_utils:service_call({fistful_stat, 'GetWithdrawals', [Req]}, Context),
    process_result(Result).

-spec list_deposits(req_data(), handler_context()) ->
    {ok, response_data()} | {error, StatError}
    when StatError ::
        {invalid | bad_token, binary()}.

list_deposits(Params, Context) ->
    Dsl = create_dsl(deposits, Params, Context),
    Req = create_request(Dsl, maps:get(continuationToken, Params, undefined)),
    Result = wapi_handler_utils:service_call({fistful_stat, 'GetDeposits', [Req]}, Context),
    process_result(Result).

create_dsl(StatTag, Req, Context) ->
    Query = create_query(StatTag, Req, Context),
    QueryParams = #{<<"size">> => genlib_map:get(limit, Req)},
    jsx:encode(#{<<"query">> => merge_and_compact(
        maps:put(genlib:to_binary(StatTag), genlib_map:compact(Query), #{}),
        QueryParams
    )}).

create_query(withdrawals, Req, Context) ->
    #{
        <<"party_id"        >> => wapi_handler_utils:get_owner(Context),
        <<"wallet_id"       >> => genlib_map:get(walletID, Req),
        <<"identity_id"     >> => genlib_map:get(identityID, Req),
        <<"withdrawal_id"   >> => genlib_map:get(withdrawalID, Req),
        <<"destination_id"  >> => genlib_map:get(destinationID, Req),
        <<"status"          >> => genlib_map:get(status, Req),
        <<"from_time"       >> => get_time(createdAtFrom, Req),
        <<"to_time"         >> => get_time(createdAtTo, Req),
        <<"amount_from"     >> => genlib_map:get(amountFrom, Req),
        <<"amount_to"       >> => genlib_map:get(amountTo, Req),
        <<"currency_code"   >> => genlib_map:get(currencyID, Req)
    };
create_query(deposits, Req, Context) ->
    #{
        <<"party_id"        >> => wapi_handler_utils:get_owner(Context),
        <<"wallet_id"       >> => genlib_map:get(walletID, Req),
        <<"identity_id"     >> => genlib_map:get(identityID, Req),
        <<"deposit_id"      >> => genlib_map:get(depositID, Req),
        <<"source_id"       >> => genlib_map:get(sourceID, Req),
        <<"status"          >> => genlib_map:get(status, Req),
        <<"from_time"       >> => get_time(createdAtFrom, Req),
        <<"to_time"         >> => get_time(createdAtTo, Req),
        <<"amount_from"     >> => genlib_map:get(amountFrom, Req),
        <<"amount_to"       >> => genlib_map:get(amountTo, Req),
        <<"currency_code"   >> => genlib_map:get(currencyID, Req)
    };
create_query(wallets, Req, Context) ->
    #{
        <<"party_id"        >> => wapi_handler_utils:get_owner(Context),
        <<"identity_id"     >> => genlib_map:get(identityID, Req),
        <<"currency_code"   >> => genlib_map:get(currencyID, Req)
    }.

create_request(Dsl, Token) ->
    #fistfulstat_StatRequest{
        dsl = Dsl,
        continuation_token = Token
    }.

process_result({ok, #fistfulstat_StatResponse{
    data = {QueryType, Data},
    continuation_token = ContinuationToken
}}) ->
    DecodedData = [unmarshal_response(QueryType, S) || S <- Data],
    Response = genlib_map:compact(#{
        <<"result">> => DecodedData,
        <<"continuationToken">> => ContinuationToken
    }),
    {ok, Response};
process_result({exception, #fistfulstat_InvalidRequest{errors = Errors}}) ->
    FormattedErrors = format_request_errors(Errors),
    {error, {invalid, FormattedErrors}};
process_result({exception, #fistfulstat_BadToken{reason = Reason}}) ->
    {error, {bad_token, Reason}}.

get_time(Key, Req) ->
    case genlib_map:get(Key, Req) of
        Timestamp when is_binary(Timestamp) ->
            wapi_utils:to_universal_time(Timestamp);
        undefined ->
            undefined
    end.

merge_and_compact(M1, M2) ->
    genlib_map:compact(maps:merge(M1, M2)).

format_request_errors([]    ) -> <<>>;
format_request_errors(Errors) -> genlib_string:join(<<"\n">>, Errors).

-spec unmarshal_response
    (withdrawals, ff_proto_fistful_stat_thrift:'StatWithdrawal'()) -> map();
    (deposits, ff_proto_fistful_stat_thrift:'StatDeposit'()) -> map();
    (wallets, ff_proto_fistful_stat_thrift:'StatWallet'()) -> map().

unmarshal_response(withdrawals, Response) ->
    merge_and_compact(#{
        <<"id"          >> => Response#fistfulstat_StatWithdrawal.id,
        <<"createdAt"   >> => Response#fistfulstat_StatWithdrawal.created_at,
        <<"wallet"      >> => Response#fistfulstat_StatWithdrawal.source_id,
        <<"destination" >> => Response#fistfulstat_StatWithdrawal.destination_id,
        <<"externalID"  >> => Response#fistfulstat_StatWithdrawal.external_id,
        <<"body"        >> => unmarshal_cash(
            Response#fistfulstat_StatWithdrawal.amount,
            Response#fistfulstat_StatWithdrawal.currency_symbolic_code
        ),
        <<"fee"         >> => unmarshal_cash(
            Response#fistfulstat_StatWithdrawal.fee,
            Response#fistfulstat_StatWithdrawal.currency_symbolic_code
        )
    }, unmarshal_withdrawal_stat_status(Response#fistfulstat_StatWithdrawal.status));
unmarshal_response(deposits, Response) ->
    merge_and_compact(#{
        <<"id"          >> => Response#fistfulstat_StatDeposit.id,
        <<"createdAt"   >> => Response#fistfulstat_StatDeposit.created_at,
        <<"wallet"      >> => Response#fistfulstat_StatDeposit.destination_id,
        <<"source"      >> => Response#fistfulstat_StatDeposit.source_id,
        <<"body"        >> => unmarshal_cash(
            Response#fistfulstat_StatDeposit.amount,
            Response#fistfulstat_StatDeposit.currency_symbolic_code
        ),
        <<"fee"         >> => unmarshal_cash(
            Response#fistfulstat_StatDeposit.fee,
            Response#fistfulstat_StatDeposit.currency_symbolic_code
        )
    }, unmarshal_deposit_stat_status(Response#fistfulstat_StatDeposit.status));
unmarshal_response(wallets, Response) ->
    genlib_map:compact(#{
        <<"id"          >> => Response#fistfulstat_StatWallet.id,
        <<"name"        >> => Response#fistfulstat_StatWallet.name,
        <<"identity"    >> => Response#fistfulstat_StatWallet.identity_id,
        <<"createdAt"   >> => Response#fistfulstat_StatWallet.created_at,
        <<"currency"    >> => Response#fistfulstat_StatWallet.currency_symbolic_code
    }).

unmarshal_cash(Amount, Currency) ->
    #{<<"amount">> => Amount, <<"currency">> => Currency}.

unmarshal_withdrawal_stat_status({pending, #fistfulstat_WithdrawalPending{}}) ->
    #{<<"status">> => <<"Pending">>};
unmarshal_withdrawal_stat_status({succeeded, #fistfulstat_WithdrawalSucceeded{}}) ->
    #{<<"status">> => <<"Succeeded">>};
unmarshal_withdrawal_stat_status({failed, #fistfulstat_WithdrawalFailed{failure = _Failure}}) ->
    #{
        <<"status">> => <<"Failed">>,
        <<"failure">> => #{<<"code">> => <<"failed">>}
    }.

unmarshal_deposit_stat_status({pending, #fistfulstat_DepositPending{}}) ->
    #{<<"status">> => <<"Pending">>};
unmarshal_deposit_stat_status({succeeded, #fistfulstat_DepositSucceeded{}}) ->
    #{<<"status">> => <<"Succeeded">>};
unmarshal_deposit_stat_status({failed, #fistfulstat_DepositFailed{failure = _Failure}}) ->
    #{
        <<"status">> => <<"Failed">>,
        <<"failure">> => #{<<"code">> => <<"failed">>}
    }.
