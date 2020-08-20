-module(wapi_stat_backend).

-include_lib("fistful_proto/include/ff_proto_fistful_stat_thrift.hrl").

-export([list_wallets/2]).
-export([list_withdrawals/2]).
-export([list_deposits/2]).

-type req_data() :: wapi_handler:req_data().
-type handler_context() :: wapi_handler:context().
% -type response_data() :: wapi_handler:response_data().
-type result_stat() :: {200 | 400, list(), map()}.

-spec list_wallets(req_data(), handler_context()) ->
    {ok, result_stat()} | {error, result_stat()}.
list_wallets(Params, Context) ->
    StatType = wallet_stat,
    Dsl = create_stat_dsl(StatType, Params, Context),
    ContinuationToken = maps:get(continuationToken, Params, undefined),
    Req = create_stat_request(Dsl, ContinuationToken),
    Result = wapi_handler_utils:service_call({fistful_stat, 'GetWallets', [Req]}, Context),
    process_stat_result(StatType, Result).

-spec list_withdrawals(req_data(), handler_context()) ->
    {ok, result_stat()} | {error, result_stat()}.
list_withdrawals(Params, Context) ->
    StatType = withdrawal_stat,
    Dsl = create_stat_dsl(StatType, Params, Context),
    ContinuationToken = maps:get(continuationToken, Params, undefined),
    Req = create_stat_request(Dsl, ContinuationToken),
    Result = wapi_handler_utils:service_call({fistful_stat, 'GetWithdrawals', [Req]}, Context),
    process_stat_result(StatType, Result).

-spec list_deposits(req_data(), handler_context()) ->
    {ok, result_stat()} | {error, result_stat()}.
list_deposits(Params, Context) ->
    StatType = deposit_stat,
    Dsl = create_stat_dsl(StatType, Params, Context),
    ContinuationToken = maps:get(continuationToken, Params, undefined),
    Req = create_stat_request(Dsl, ContinuationToken),
    Result = wapi_handler_utils:service_call({fistful_stat, 'GetDeposits', [Req]}, Context),
    process_stat_result(StatType, Result).

create_stat_dsl(withdrawal_stat, Req, Context) ->
    Query = #{
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
    },
    QueryParams = #{<<"size">> => genlib_map:get(limit, Req)},
    jsx:encode(create_dsl(withdrawals, Query, QueryParams));
create_stat_dsl(deposit_stat, Req, Context) ->
    Query = #{
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
    },
    QueryParams = #{<<"size">> => genlib_map:get(limit, Req)},
    jsx:encode(create_dsl(deposits, Query, QueryParams));
create_stat_dsl(wallet_stat, Req, Context) ->
    Query = #{
        <<"party_id"        >> => wapi_handler_utils:get_owner(Context),
        <<"identity_id"     >> => genlib_map:get(identityID, Req),
        <<"currency_code"   >> => genlib_map:get(currencyID, Req)
    },
    QueryParams = #{<<"size">> => genlib_map:get(limit, Req)},
    jsx:encode(create_dsl(wallets, Query, QueryParams)).

create_stat_request(Dsl, Token) ->
    #fistfulstat_StatRequest{
        dsl = Dsl,
        continuation_token = Token
    }.

process_stat_result(StatType, Result) ->
    case Result of
        {ok, #fistfulstat_StatResponse{
            data = {_QueryType, Data},
            continuation_token = ContinuationToken
        }} ->
            DecodedData = [decode_stat(StatType, S) || S <- Data],
            Responce = genlib_map:compact(#{
                <<"result">> => DecodedData,
                <<"continuationToken">> => ContinuationToken
            }),
            {ok, {200, [], Responce}};
        {exception, #fistfulstat_InvalidRequest{errors = Errors}} ->
            FormattedErrors = format_request_errors(Errors),
            {error, {400, [], bad_request_error(invalidRequest, FormattedErrors)}};
        {exception, #fistfulstat_BadToken{reason = Reason}} ->
            {error, {400, [], bad_request_error(invalidRequest, Reason)}}
    end.

get_time(Key, Req) ->
    case genlib_map:get(Key, Req) of
        Timestamp when is_binary(Timestamp) ->
            wapi_utils:to_universal_time(Timestamp);
        undefined ->
            undefined
    end.

create_dsl(StatTag, Query, QueryParams) ->
    #{<<"query">> => merge_and_compact(
        maps:put(genlib:to_binary(StatTag), genlib_map:compact(Query), #{}),
        QueryParams
    )}.

merge_and_compact(M1, M2) ->
    genlib_map:compact(maps:merge(M1, M2)).

bad_request_error(Type, Name) ->
    #{<<"errorType">> => genlib:to_binary(Type), <<"name">> => genlib:to_binary(Name)}.

format_request_errors([]    ) -> <<>>;
format_request_errors(Errors) -> genlib_string:join(<<"\n">>, Errors).

decode_stat(withdrawal_stat, Response) ->
    merge_and_compact(#{
        <<"id"          >> => Response#fistfulstat_StatWithdrawal.id,
        <<"createdAt"   >> => Response#fistfulstat_StatWithdrawal.created_at,
        <<"wallet"      >> => Response#fistfulstat_StatWithdrawal.source_id,
        <<"destination" >> => Response#fistfulstat_StatWithdrawal.destination_id,
        <<"externalID"  >> => Response#fistfulstat_StatWithdrawal.external_id,
        <<"body"        >> => decode_stat_cash(
            Response#fistfulstat_StatWithdrawal.amount,
            Response#fistfulstat_StatWithdrawal.currency_symbolic_code
        ),
        <<"fee"         >> => decode_stat_cash(
            Response#fistfulstat_StatWithdrawal.fee,
            Response#fistfulstat_StatWithdrawal.currency_symbolic_code
        )
    }, decode_withdrawal_stat_status(Response#fistfulstat_StatWithdrawal.status));
decode_stat(deposit_stat, Response) ->
    merge_and_compact(#{
        <<"id"          >> => Response#fistfulstat_StatDeposit.id,
        <<"createdAt"   >> => Response#fistfulstat_StatDeposit.created_at,
        <<"wallet"      >> => Response#fistfulstat_StatDeposit.destination_id,
        <<"source"      >> => Response#fistfulstat_StatDeposit.source_id,
        <<"body"        >> => decode_stat_cash(
            Response#fistfulstat_StatDeposit.amount,
            Response#fistfulstat_StatDeposit.currency_symbolic_code
        ),
        <<"fee"         >> => decode_stat_cash(
            Response#fistfulstat_StatDeposit.fee,
            Response#fistfulstat_StatDeposit.currency_symbolic_code
        )
    }, decode_deposit_stat_status(Response#fistfulstat_StatDeposit.status));
decode_stat(wallet_stat, Response) ->
    genlib_map:compact(#{
        <<"id"          >> => Response#fistfulstat_StatWallet.id,
        <<"name"        >> => Response#fistfulstat_StatWallet.name,
        <<"identity"    >> => Response#fistfulstat_StatWallet.identity_id,
        <<"createdAt"   >> => Response#fistfulstat_StatWallet.created_at,
        <<"currency"    >> => Response#fistfulstat_StatWallet.currency_symbolic_code
    }).

decode_stat_cash(Amount, Currency) ->
    #{<<"amount">> => Amount, <<"currency">> => Currency}.

decode_withdrawal_stat_status({pending, #fistfulstat_WithdrawalPending{}}) ->
    #{<<"status">> => <<"Pending">>};
decode_withdrawal_stat_status({succeeded, #fistfulstat_WithdrawalSucceeded{}}) ->
    #{<<"status">> => <<"Succeeded">>};
decode_withdrawal_stat_status({failed, #fistfulstat_WithdrawalFailed{failure = _Failure}}) ->
    #{
        <<"status">> => <<"Failed">>,
        <<"failure">> => #{<<"code">> => <<"failed">>}
    }.

decode_deposit_stat_status({pending, #fistfulstat_DepositPending{}}) ->
    #{<<"status">> => <<"Pending">>};
decode_deposit_stat_status({succeeded, #fistfulstat_DepositSucceeded{}}) ->
    #{<<"status">> => <<"Succeeded">>};
decode_deposit_stat_status({failed, #fistfulstat_DepositFailed{failure = _Failure}}) ->
    #{
        <<"status">> => <<"Failed">>,
        <<"failure">> => #{<<"code">> => <<"failed">>}
    }.
