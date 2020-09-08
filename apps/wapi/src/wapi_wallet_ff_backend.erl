-module(wapi_wallet_ff_backend).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_fistful_stat_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_webhooker_thrift.hrl").
-include_lib("file_storage_proto/include/fs_file_storage_thrift.hrl").
-include_lib("fistful_reporter_proto/include/ff_reporter_reports_thrift.hrl").

%% API
-export([get_providers/2]).
-export([get_provider/2]).
-export([get_provider_identity_classes/2]).
-export([get_provider_identity_class/3]).
-export([get_provider_identity_class_levels/3]).
-export([get_provider_identity_class_level/4]).

-export([get_identities/2]).
-export([get_identity/2]).
-export([create_identity/2]).
-export([get_identity_challenges/3]).
-export([create_identity_challenge/3]).
-export([get_identity_challenge/3]).
-export([get_identity_challenge_events/2]).
-export([get_identity_challenge_event/2]).

-export([get_wallet/2]).
-export([get_wallet_by_external_id/2]).
-export([create_wallet/2]).
-export([get_wallet_account/2]).
-export([list_wallets/2]).

-export([get_destinations/2]).
-export([get_destination/2]).
-export([get_destination_by_external_id/2]).
-export([create_destination/2]).

-export([create_withdrawal/2]).
-export([get_withdrawal/2]).
-export([get_withdrawal_by_external_id/2]).

-export([get_withdrawal_events/2]).
-export([get_withdrawal_event/3]).
-export([list_withdrawals/2]).
-export([create_quote/2]).

-export([get_residence/2]).
-export([get_currency/2]).

-export([create_report/2]).
-export([get_report/3]).
-export([get_reports/2]).
-export([download_file/3]).

-export([list_deposits/2]).

-export([create_webhook/2]).
-export([get_webhooks/2]).
-export([get_webhook/3]).
-export([delete_webhook/3]).

-export([quote_p2p_transfer/2]).
-export([create_p2p_transfer/2]).
-export([get_p2p_transfer/2]).
-export([get_p2p_transfer_events/2]).

-export([create_p2p_template/2]).
-export([get_p2p_template/2]).
-export([block_p2p_template/2]).
-export([issue_p2p_template_access_token/3]).
-export([issue_p2p_transfer_ticket/3]).

-export([create_p2p_transfer_with_template/3]).
-export([quote_p2p_transfer_with_template/3]).

-export([create_w2w_transfer/2]).
-export([get_w2w_transfer/2]).

%% Types

-type ctx()         :: wapi_handler:context().
-type params()      :: map().
-type id()          :: binary() | undefined.
-type external_id() :: binary().
-type result()      :: result(map()).
-type result(T)     :: result(T, notfound).
-type result(T, E)  :: {ok, T} | {error, E}.
-type result_stat() :: {200 | 400, list(), map()}.

-define(CTX_NS, <<"com.rbkmoney.wapi">>).
-define(PARAMS_HASH, <<"params_hash">>).
-define(EXTERNAL_ID, <<"externalID">>).
-define(SIGNEE, wapi).
-define(BENDER_DOMAIN, <<"wapi">>).
-define(DEFAULT_EVENTS_LIMIT, 50).
-define(DOMAIN, <<"wallet-api">>).

-dialyzer([{nowarn_function, [to_swag/2]}]).

%% API

%% Providers

-spec get_providers([binary()], ctx()) -> [map()].
get_providers(Residences, _Context) ->
    ResidenceSet = ordsets:from_list(from_swag({list, residence}, Residences)),
    to_swag({list, provider}, [P ||
        P <- ff_provider:list(),
        ordsets:is_subset(
            ResidenceSet,
            ordsets:from_list(ff_provider:residences(P))
        )
    ]).

-spec get_provider(id(), ctx()) -> result().
get_provider(ProviderId, _Context) ->
    do(fun() -> to_swag(provider, unwrap(ff_provider:get(ProviderId))) end).

-spec get_provider_identity_classes(id(), ctx()) -> result([map()]).
get_provider_identity_classes(Id, _Context) ->
    do(fun() ->
        Provider = unwrap(ff_provider:get(Id)),
        lists:map(
            fun(ClassId) -> get_provider_identity_class(ClassId, Provider) end,
            ff_provider:list_identity_classes(Provider)
        )
    end).

-spec get_provider_identity_class(id(), id(), ctx()) -> result().
get_provider_identity_class(ProviderId, ClassId, _Context) ->
    do(fun() -> get_provider_identity_class(ClassId, unwrap(ff_provider:get(ProviderId))) end).

get_provider_identity_class(ClassId, Provider) ->
    to_swag(identity_class, unwrap(ff_provider:get_identity_class(ClassId, Provider))).

-spec get_provider_identity_class_levels(id(), id(), ctx()) -> no_return().
get_provider_identity_class_levels(_ProviderId, _ClassId, _Context) ->
    not_implemented().

-spec get_provider_identity_class_level(id(), id(), id(), ctx()) -> no_return().
get_provider_identity_class_level(_ProviderId, _ClassId, _LevelId, _Context) ->
    not_implemented().

%% Identities

-spec get_identities(params(), ctx()) -> no_return().
get_identities(_Params, _Context) ->
    not_implemented().

-spec get_identity(id(), ctx()) -> result(map(),
    {identity, notfound}     |
    {identity, unauthorized}
).
get_identity(IdentityId, Context) ->
    do(fun() -> to_swag(identity, get_state(identity, IdentityId, Context)) end).

-spec create_identity(params(), ctx()) -> result(map(),
    {provider, notfound}       |
    {identity_class, notfound} |
    {inaccessible, ff_party:inaccessibility()} |
    {email, notfound}          |
    {external_id_conflict, id(), external_id()}
).
create_identity(Params, Context) ->
    IdentityParams = from_swag(identity_params, Params),
    CreateIdentity = fun(ID, EntityCtx) ->
        ff_identity_machine:create(
            maps:merge(IdentityParams#{id => ID}, #{party => wapi_handler_utils:get_owner(Context)}),
            add_meta_to_ctx([<<"name">>], Params, EntityCtx)
        )
    end,
    CreateFun = fun(ID, EntityCtx) -> with_party(Context, fun() -> CreateIdentity(ID, EntityCtx) end) end,
    do(fun() -> unwrap(create_entity(identity, Params, CreateFun, Context)) end).

-spec get_identity_challenges(id(), [binary()], ctx()) -> result(map(),
    {identity, notfound}     |
    {identity, unauthorized}
).
get_identity_challenges(IdentityId, Statuses, Context) ->
    do(fun() ->
        Challenges0 = maps:to_list(ff_identity:challenges(
            ff_identity_machine:identity(get_state(identity, IdentityId, Context))
        )),
        to_swag({list, identity_challenge}, [
            {Id, C, enrich_proofs(ff_identity_challenge:proofs(C), Context)} ||
                {Id, C} <- Challenges0,
                Status  <- [ff_identity_challenge:status(C)],
                lists:all(
                    fun (F) -> filter_identity_challenge_status(F, Status) end,
                    Statuses
                )
        ])
    end).

-spec create_identity_challenge(id(), params(), ctx()) -> result(map(),
    {identity, notfound}               |
    {identity, unauthorized}           |
    {challenge, {pending, _}}          |
    {challenge, {class, notfound}}     |
    {challenge, {proof, notfound}}     |
    {challenge, {proof, insufficient}} |
    {challenge, {level, _}}            |
    {challenge, conflict}
).
create_identity_challenge(IdentityId, Params, Context) ->
    Type          = identity_challenge,
    Hash          = erlang:phash2(Params),
    {ok, ChallengeID} = gen_id(Type, undefined, Hash, Context),
    do(fun() ->
        _ = check_resource(identity, IdentityId, Context),
        ok = unwrap(ff_identity_machine:start_challenge(IdentityId,
            maps:merge(#{id => ChallengeID}, from_swag(identity_challenge_params, Params)
        ))),
        unwrap(get_identity_challenge(IdentityId, ChallengeID, Context))
    end).

-spec get_identity_challenge(id(), id(), ctx()) -> result(map(),
    {identity, notfound}     |
    {identity, unauthorized} |
    {challenge, notfound}
).
get_identity_challenge(IdentityId, ChallengeId, Context) ->
    do(fun() ->
        Challenge = unwrap(challenge, ff_identity:challenge(
            ChallengeId, ff_identity_machine:identity(get_state(identity, IdentityId, Context))
        )),
        Proofs = enrich_proofs(ff_identity_challenge:proofs(Challenge), Context),
        to_swag(identity_challenge, {ChallengeId, Challenge, Proofs})
    end).

-spec get_identity_challenge_events(params(), ctx()) -> result([map()],
    {identity, notfound}     |
    {identity, unauthorized}
).
get_identity_challenge_events(Params = #{
    'identityID'  := IdentityId,
    'challengeID' := ChallengeId,
    'limit'  := Limit
}, Context) ->
    Cursor = genlib_map:get('eventCursor', Params),
    Filter = fun
        ({ID, {ev, Ts, {{challenge, I}, Body = {status_changed, _}}}}) when I =:= ChallengeId ->
            {true, {ID, Ts, Body}};
        (_) ->
            false
    end,
    get_swag_events({identity, challenge_event}, IdentityId, Limit, Cursor, Filter, Context).

-spec get_identity_challenge_event(params(), ctx()) -> result(map(),
    {identity, notfound}     |
    {identity, unauthorized} |
    {event, notfound}
).
get_identity_challenge_event(#{
    'identityID'  := IdentityId,
    'challengeID' := ChallengeId,
    'eventID'     := EventId
}, Context) ->
    Mapper = fun
        ({ID, {ev, Ts, {{challenge, I}, Body = {status_changed, _}}}}) when I =:= ChallengeId andalso ID =:= EventId ->
            {true, {ID, Ts, Body}};
        (_) ->
            false
    end,
    get_swag_event({identity, challenge_event}, IdentityId, EventId, Mapper, Context).

%% Wallets

-spec get_wallet(id(), ctx()) -> result(map(),
    {wallet, notfound}     |
    {wallet, unauthorized}
).
get_wallet(WalletID, Context) ->
    do(fun() -> to_swag(wallet, get_state(wallet, WalletID, Context)) end).

-spec get_wallet_by_external_id(external_id(), ctx()) -> result(map(),
    {wallet, notfound}     |
    {wallet, unauthorized}
).
get_wallet_by_external_id(ExternalID, #{woody_context := WoodyContext} = Context) ->
    AuthContext = wapi_handler_utils:get_auth_context(Context),
    PartyID = uac_authorizer_jwt:get_subject_id(AuthContext),
    IdempotentKey = wapi_backend_utils:get_idempotent_key(wallet, PartyID, ExternalID),
    case bender_client:get_internal_id(IdempotentKey, WoodyContext) of
        {ok, {WalletID, _}, _} -> get_wallet(WalletID, Context);
        {error, internal_id_not_found} -> {error, {wallet, notfound}}
    end.

-spec create_wallet(params(), ctx()) -> result(map(),
    invalid                                     |
    {identity, unauthorized}                    |
    {external_id_conflict, id(), external_id()} |
    {inaccessible, _}                           |
    ff_wallet:create_error()
).
create_wallet(Params = #{<<"identity">> := IdenityId}, Context) ->
    WalletParams = from_swag(wallet_params, Params),
    CreateFun = fun(ID, EntityCtx) ->
            _ = check_resource(identity, IdenityId, Context),
            ff_wallet_machine:create(
                WalletParams#{id => ID},
                add_meta_to_ctx([], Params, EntityCtx)
            )
    end,
    do(fun() -> unwrap(create_entity(wallet, Params, CreateFun, Context)) end).

-spec get_wallet_account(id(), ctx()) -> result(map(),
    {wallet, notfound}     |
    {wallet, unauthorized}
).
get_wallet_account(WalletID, Context) ->
    do(fun () ->
        Account = ff_wallet:account(ff_wallet_machine:wallet(get_state(wallet, WalletID, Context))),
        {Amounts, Currency} = unwrap(ff_transaction:balance(
            Account,
            ff_clock:latest_clock()
        )),
        to_swag(wallet_account, {ff_indef:current(Amounts), ff_indef:expmin(Amounts), Currency})
    end).

-spec list_wallets(params(), ctx()) ->
    {ok, result_stat()} | {error, result_stat()}.
list_wallets(Params, Context) ->
    StatType = wallet_stat,
    Dsl = create_stat_dsl(StatType, Params, Context),
    ContinuationToken = maps:get(continuationToken, Params, undefined),
    Req = create_stat_request(Dsl, ContinuationToken),
    Result = wapi_handler_utils:service_call({fistful_stat, 'GetWallets', [Req]}, Context),
    process_stat_result(StatType, Result).

%% Withdrawals

-spec get_destinations(params(), ctx()) -> no_return().
get_destinations(_Params, _Context) ->
    not_implemented().

-spec get_destination(id(), ctx()) -> result(map(),
    {destination, notfound}     |
    {destination, unauthorized}
).
get_destination(DestinationID, Context) ->
    do(fun() -> to_swag(destination, get_state(destination, DestinationID, Context)) end).

-spec get_destination_by_external_id(id(), ctx()) -> result(map(),
    {destination, unauthorized} |
    {destination, notfound}     |
    {external_id, {unknown_external_id, id()}}
).
get_destination_by_external_id(ExternalID, Context = #{woody_context := WoodyCtx}) ->
    PartyID = wapi_handler_utils:get_owner(Context),
    IdempotentKey = wapi_backend_utils:get_idempotent_key(destination, PartyID, ExternalID),
    case bender_client:get_internal_id(IdempotentKey, WoodyCtx) of
        {ok, {DestinationID, _}, _CtxData} ->
            get_destination(DestinationID, Context);
        {error, internal_id_not_found} ->
            {error, {external_id, {unknown_external_id, ExternalID}}}
    end.

-spec create_destination(params(), ctx()) -> result(map(),
    invalid                                     |
    {invalid_resource_token, _}                 |
    {identity, unauthorized}                    |
    {identity, notfound}                        |
    {currency, notfound}                        |
    {inaccessible, _}                           |
    {external_id_conflict, id(), external_id()} |
    {illegal_pattern, _}
).
create_destination(Params = #{<<"identity">> := IdenityId}, Context) ->
    CreateFun = fun(ID, EntityCtx) ->
        do(fun() ->
            _ = check_resource(identity, IdenityId, Context),
            DestinationParams = from_swag(destination_params, Params),
            Resource = unwrap(construct_resource(maps:get(resource, DestinationParams))),
            unwrap(ff_destination:create(
                DestinationParams#{id => ID, resource => Resource},
                add_meta_to_ctx([], Params, EntityCtx)
            ))
        end)
    end,
    do(fun() -> unwrap(create_entity(destination, Params, CreateFun, Context)) end).

-spec create_withdrawal(params(), ctx()) -> result(map(),
    {source, notfound}            |
    {destination, notfound}       |
    {destination, unauthorized}   |
    {external_id_conflict, id(), external_id()} |
    {provider, notfound}          |
    {wallet, {inaccessible, _}}   |
    {wallet, {currency, invalid}} |
    {wallet, {provider, invalid}} |
    {quote_invalid_party, _}      |
    {quote_invalid_wallet, _}     |
    {quote, {invalid_body, _}}    |
    {quote, {invalid_destination, _}} |
    {terms, {terms_violation, _}} |
    {identity_providers_mismatch, {ff_provider:id(), ff_provider:id()}} |
    {destination_resource, {bin_data, ff_bin_data:bin_data_error()}} |
    {quote, token_expired} |
    {Resource, {unauthorized, _}}
) when Resource :: wallet | destination.
create_withdrawal(Params, Context) ->
    CreateFun = fun(ID, EntityCtx) ->
        do(fun() ->
            _ = authorize_withdrawal(Params, Context),
            Quote = unwrap(maybe_check_quote_token(Params, Context)),
            WithdrawalParams = from_swag(withdrawal_params, Params),
            unwrap(ff_withdrawal_machine:create(
                genlib_map:compact(WithdrawalParams#{id => ID, quote => Quote}),
                add_meta_to_ctx([], Params, EntityCtx)
            ))
        end)
    end,
    do(fun() -> unwrap(create_entity(withdrawal, Params, CreateFun, Context)) end).

-spec get_withdrawal(id(), ctx()) -> result(map(),
    {withdrawal, unauthorized} |
    {withdrawal, {unknown_withdrawal, ff_withdrawal:id()}}
).
get_withdrawal(WithdrawalId, Context) ->
    do(fun() -> to_swag(withdrawal, get_state(withdrawal, WithdrawalId, Context)) end).

-spec get_withdrawal_by_external_id(id(), ctx()) -> result(map(),
    {withdrawal, unauthorized} |
    {withdrawal, {unknown_withdrawal, ff_withdrawal:id()}} |
    {external_id, {unknown_external_id, id()}}
).
get_withdrawal_by_external_id(ExternalID, Context = #{woody_context := WoodyCtx}) ->
    PartyID = wapi_handler_utils:get_owner(Context),
    IdempotentKey = wapi_backend_utils:get_idempotent_key(withdrawal, PartyID, ExternalID),
    case bender_client:get_internal_id(IdempotentKey, WoodyCtx) of
        {ok, {WithdrawalId, _}, _CtxData} ->
            get_withdrawal(WithdrawalId, Context);
        {error, internal_id_not_found} ->
            {error, {external_id, {unknown_external_id, ExternalID}}}
    end.

-spec get_withdrawal_events(params(), ctx()) -> result([map()],
    {withdrawal, unauthorized} |
    {withdrawal, {unknown_withdrawal, ff_withdrawal:id()}}
).
get_withdrawal_events(Params = #{'withdrawalID' := WithdrawalId, 'limit' := Limit}, Context) ->
    Cursor = genlib_map:get('eventCursor', Params),
    Filter = fun
        ({ID, {ev, Ts, Body = {status_changed, _}}}) ->
            {true, {ID, Ts, Body}};
        (_) ->
            false
    end,
    get_swag_events({withdrawal, event}, WithdrawalId, Limit, Cursor, Filter, Context).

-spec get_withdrawal_event(id(), integer(), ctx()) -> result(map(),
    {withdrawal, unauthorized} |
    {withdrawal, {unknown_withdrawal, ff_withdrawal:id()}} |
    {event, notfound}
).
get_withdrawal_event(WithdrawalId, EventId, Context) ->
    Mapper = fun
        ({ID, {ev, Ts, Body = {status_changed, _}}}) when ID =:= EventId ->
            {true, {ID, Ts, Body}};
        (_) ->
            false
    end,
    get_swag_event({withdrawal, event}, WithdrawalId, EventId, Mapper, Context).

-spec list_withdrawals(params(), ctx()) ->
    {ok, result_stat()} | {error, result_stat()}.
list_withdrawals(Params, Context) ->
    StatType = withdrawal_stat,
    Dsl = create_stat_dsl(StatType, Params, Context),
    ContinuationToken = maps:get(continuationToken, Params, undefined),
    Req = create_stat_request(Dsl, ContinuationToken),
    Result = wapi_handler_utils:service_call({fistful_stat, 'GetWithdrawals', [Req]}, Context),
    process_stat_result(StatType, Result).

-spec create_quote(params(), ctx()) -> result(map(),
    {destination, notfound}       |
    {destination, unauthorized}   |
    {route, _Reason}              |
    {identity_providers_mismatch, {ff_provider:id(), ff_provider:id()}} |
    {wallet, notfound}
).
create_quote(#{'WithdrawalQuoteParams' := Params}, Context) ->
    do(fun () ->
        CreateQuoteParams = from_swag(create_quote_params, Params),
        Quote = unwrap(ff_withdrawal:get_quote(CreateQuoteParams)),
        Token = create_quote_token(
            Quote,
            maps:get(<<"walletID">>, Params),
            maps:get(<<"destinationID">>, Params, undefined),
            wapi_handler_utils:get_owner(Context)
        ),
        to_swag(quote, {Quote, Token})
    end).

%% Residences

-spec get_residence(binary(), ctx()) -> result().
get_residence(Residence, _Context) ->
    do(fun () ->
        to_swag(residence_object, unwrap(ff_residence:get(from_swag(residence, Residence))))
    end).

%% Currencies

-spec get_currency(binary(), ctx()) -> result().
get_currency(CurrencyId, _Context) ->
    do(fun () ->
        to_swag(currency_object, unwrap(ff_currency:get(from_swag(currency, CurrencyId))))
    end).

%% Reports

-spec create_report(params(), ctx()) -> result(map(),
    {identity, unauthorized}    |
    {identity, notfound}        |
    invalid_request             |
    invalid_contract
).
create_report(#{
    identityID     := IdentityID,
    'ReportParams' := ReportParams
}, Context) ->
    do(fun () ->
        ContractID = get_contract_id_from_identity(IdentityID, Context),
        Req = create_report_request(#{
            party_id     => wapi_handler_utils:get_owner(Context),
            contract_id  => ContractID,
            from_time    => get_time(<<"fromTime">>, ReportParams),
            to_time      => get_time(<<"toTime">>, ReportParams)
        }),
        Call = {fistful_report, 'GenerateReport', [Req, maps:get(<<"reportType">>, ReportParams)]},
        case wapi_handler_utils:service_call(Call, Context) of
            {ok, ReportID} ->
                unwrap(get_report(contractID, ReportID, ContractID, Context));
            {exception, #'InvalidRequest'{}} ->
                throw(invalid_request);
            {exception, #ff_reports_ContractNotFound{}} ->
                throw(invalid_contract)
        end
    end).

-spec get_report(integer(), binary(), ctx()) -> result(map(),
    {identity, unauthorized}    |
    {identity, notfound}        |
    notfound
).
get_report(ReportID, IdentityID, Context) ->
    get_report(identityID, ReportID, IdentityID, Context).

get_report(identityID, ReportID, IdentityID, Context) ->
    do(fun () ->
        ContractID = get_contract_id_from_identity(IdentityID, Context),
        unwrap(get_report(contractID, ReportID, ContractID, Context))
    end);
get_report(contractID, ReportID, ContractID, Context) ->
    do(fun () ->
        PartyID = wapi_handler_utils:get_owner(Context),
        Call = {fistful_report, 'GetReport', [PartyID, ContractID, ReportID]},
        case wapi_handler_utils:service_call(Call, Context) of
            {ok, Report} ->
                to_swag(report_object, Report);
            {exception, #ff_reports_ReportNotFound{}} ->
                throw(notfound)
        end
    end).

-spec get_reports(params(), ctx()) -> result(map(),
    {identity, unauthorized}    |
    {identity, notfound}        |
    invalid_request             |
    {dataset_too_big, integer()}
).
get_reports(#{
    identityID   := IdentityID
} = Params, Context) ->
    do(fun () ->
        ContractID = get_contract_id_from_identity(IdentityID, Context),
        Req = create_report_request(#{
            party_id     => wapi_handler_utils:get_owner(Context),
            contract_id  => ContractID,
            from_time    => get_time(fromTime, Params),
            to_time      => get_time(toTime, Params)
        }),
        Call = {fistful_report, 'GetReports', [Req, [genlib:to_binary(maps:get(type, Params))]]},
        case wapi_handler_utils:service_call(Call, Context) of
            {ok, ReportList} ->
                to_swag({list, report_object}, ReportList);
            {exception, #'InvalidRequest'{}} ->
                throw(invalid_request);
            {exception, #ff_reports_DatasetTooBig{limit = Limit}} ->
                throw({dataset_too_big, Limit})
        end
    end).

-spec download_file(binary(), binary(), ctx()) -> result().
download_file(FileID, ExpiresAt, Context) ->
    Timestamp = wapi_utils:to_universal_time(ExpiresAt),
    Call = {file_storage, 'GenerateDownloadUrl', [FileID, Timestamp]},
    case wapi_handler_utils:service_call(Call, Context) of
        {exception, #file_storage_FileNotFound{}} ->
            {error, notfound};
        Result->
            Result
    end.

%% Deposits

-spec list_deposits(params(), ctx()) ->
    {ok, result_stat()} | {error, result_stat()}.
list_deposits(Params, Context) ->
    StatType = deposit_stat,
    Dsl = create_stat_dsl(StatType, Params, Context),
    ContinuationToken = maps:get(continuationToken, Params, undefined),
    Req = create_stat_request(Dsl, ContinuationToken),
    Result = wapi_handler_utils:service_call({fistful_stat, 'GetDeposits', [Req]}, Context),
    process_stat_result(StatType, Result).

%% Webhooks

-spec create_webhook(params(), ctx()) -> result(map(),
    {identity, notfound} |
    {identity, unauthorized} |
    {wallet, notfound} |
    {wallet, unauthorized}
).
create_webhook(Params, Context) ->
    do(fun () ->
        NewParams = #{
            identity_id := IdentityID,
            scope := EventFilter,
            url := URL
        } = from_swag(webhook_params, maps:get('Webhook', Params)),
        WalletID = maps:get(wallet_id, NewParams, undefined),
        case WalletID /= undefined of
            true ->
                _ = check_resource(wallet, WalletID, Context);
            false ->
                ok
        end,
        _ = check_resource(identity, IdentityID, Context),
        WebhookParams = #webhooker_WebhookParams{
            identity_id = IdentityID,
            wallet_id = WalletID,
            event_filter = EventFilter,
            url = URL
        },
        Call = {webhook_manager, 'Create', [WebhookParams]},
        {ok, NewWebhook} = wapi_handler_utils:service_call(Call, Context),
        to_swag(webhook, NewWebhook)
    end).

-spec get_webhooks(id(), ctx()) -> result(list(map()),
    {identity, notfound} |
    {identity, unauthorized}
).
get_webhooks(IdentityID, Context) ->
    do(fun () ->
        _ = check_resource(identity, IdentityID, Context),
        Call = {webhook_manager, 'GetList', [IdentityID]},
        {ok, Webhooks} = wapi_handler_utils:service_call(Call, Context),
        to_swag({list, webhook}, Webhooks)
    end).

-spec get_webhook(id(), id(), ctx()) -> result(map(),
    notfound |
    {identity, notfound} |
    {identity, unauthorized}
).
get_webhook(WebhookID, IdentityID, Context) ->
    do(fun () ->
        EncodedID = encode_webhook_id(WebhookID),
        _ = check_resource(identity, IdentityID, Context),
        Call = {webhook_manager, 'Get', [EncodedID]},
        case wapi_handler_utils:service_call(Call, Context) of
            {ok, Webhook} ->
                to_swag(webhook, Webhook);
            {exception, #webhooker_WebhookNotFound{}} ->
                throw(notfound)
        end
    end).

-spec delete_webhook(id(), id(), ctx()) ->
    ok |
    {error,
        notfound |
        {identity, notfound} |
        {identity, unauthorized}
    }.
delete_webhook(WebhookID, IdentityID, Context) ->
    do(fun () ->
        EncodedID = encode_webhook_id(WebhookID),
        _ = check_resource(identity, IdentityID, Context),
        Call = {webhook_manager, 'Delete', [EncodedID]},
        case wapi_handler_utils:service_call(Call, Context) of
            {ok, _} ->
                ok;
            {exception, #webhooker_WebhookNotFound{}} ->
                throw(notfound)
        end
    end).

%% P2P

-spec quote_p2p_transfer(params(), ctx()) -> result(map(),
    {invalid_resource_token, _} |
    p2p_quote:get_quote_error()
).
quote_p2p_transfer(Params, Context) ->
    do(fun () ->
        #{
            sender := Sender,
            receiver := Receiver,
            identity_id := IdentityID,
            body := Body
        } = from_swag(quote_p2p_params, Params),
        PartyID = wapi_handler_utils:get_owner(Context),
        SenderResource = unwrap(construct_resource(Sender)),
        ReceiverResource = unwrap(construct_resource(Receiver)),
        Quote = unwrap(p2p_quote:get(#{
            body => Body,
            identity_id => IdentityID,
            sender => SenderResource,
            receiver => ReceiverResource
        })),
        Token = create_p2p_quote_token(Quote, PartyID),
        ExpiresOn = p2p_quote:expires_on(Quote),
        SurplusCash = get_p2p_quote_surplus(Quote),
        to_swag(p2p_transfer_quote, {SurplusCash, Token, ExpiresOn})
    end).

-spec create_p2p_transfer(params(), ctx()) -> result(map(),
    p2p_transfer:create_error() |
    {identity, unauthorized} |
    {external_id_conflict, id(), external_id()} |
    {invalid_resource_token, _} |
    {quote, token_expired} |
    {token,
        {unsupported_version, integer() | undefined} |
        {not_verified, invalid_signature} |
        {not_verified, identity_mismatch}
    }
).
create_p2p_transfer(Params = #{<<"identityID">> := IdentityId}, Context) ->
    CreateFun =
        fun(ID, EntityCtx) ->
            do(fun() ->
                _ = check_resource(identity, IdentityId, Context),
                ParsedParams = unwrap(maybe_add_p2p_quote_token(
                    from_swag(create_p2p_params, Params)
                )),
                SenderResource = unwrap(construct_resource(maps:get(sender, ParsedParams))),
                ReceiverResource = unwrap(construct_resource(maps:get(receiver, ParsedParams))),
                RawSenderResource = {raw, #{
                    resource_params => SenderResource,
                    contact_info => maps:get(contact_info, ParsedParams)
                }},
                RawReceiverResource = {raw, #{resource_params => ReceiverResource, contact_info => #{}}},
                unwrap(p2p_transfer_machine:create(
                    genlib_map:compact(ParsedParams#{
                        id => ID,
                        sender => RawSenderResource,
                        receiver => RawReceiverResource
                    }),
                    add_meta_to_ctx([], Params, EntityCtx)
                ))
            end)
        end,
    do(fun () -> unwrap(create_entity(p2p_transfer, Params, CreateFun, Context)) end).

-spec get_p2p_transfer(params(), ctx()) -> result(map(),
    {p2p_transfer, unauthorized} |
    {p2p_transfer, {unknown_p2p_transfer, binary()}}
).
get_p2p_transfer(ID, Context) ->
    do(fun () ->
        State = get_state(p2p_transfer, ID, Context),
        to_swag(p2p_transfer, State)
    end).

-spec get_p2p_transfer_events({id(), binary() | undefined}, ctx()) -> result(map(),
    {p2p_transfer, unauthorized} |
    {p2p_transfer, not_found} |
    {token,
        {unsupported_version, integer() | undefined} |
        {not_verified, invalid_signature}
    }
).
get_p2p_transfer_events({ID, CT}, Context) ->
    do(fun () ->
        PartyID = wapi_handler_utils:get_owner(Context),
        DecodedCT = unwrap(prepare_p2p_transfer_event_continuation_token(PartyID, CT)),
        P2PTransferEventID = maps:get(p2p_transfer_event_id, DecodedCT, undefined),
        P2PSessionEventID = maps:get(p2p_session_event_id, DecodedCT, undefined),
        Limit = genlib_app:env(wapi, events_fetch_limit, ?DEFAULT_EVENTS_LIMIT),
        {P2PSessionEvents, P2PSessionEventsLastID} =
            unwrap(maybe_get_session_events(ID, Limit, P2PSessionEventID, Context)),
        {P2PTransferEvents, P2PTransferEventsLastID} =
            unwrap(maybe_get_transfer_events(ID, Limit, P2PTransferEventID, Context)),
        MixedEvents = mix_events([P2PTransferEvents, P2PSessionEvents]),
        ContinuationToken = create_p2p_transfer_events_continuation_token(#{
            p2p_transfer_event_id => max_event_id(P2PTransferEventsLastID, P2PTransferEventID),
            p2p_session_event_id => max_event_id(P2PSessionEventsLastID, P2PSessionEventID)
        }, Context),
        to_swag(p2p_transfer_events, {MixedEvents, ContinuationToken})
    end).

%% P2P Templates

-spec create_p2p_template(params(), ctx()) -> result(map(),
    p2p_template:create_error() |
    {external_id_conflict, id(), external_id()} |
    {identity, unauthorized}
).
create_p2p_template(Params = #{<<"identityID">> := IdentityId}, Context) ->
    CreateFun =
        fun(ID, EntityCtx) ->
            do(fun() ->
                _ = check_resource(identity, IdentityId, Context),
                ParsedParams = from_swag(p2p_template_create_params, Params),
                unwrap(p2p_template_machine:create(
                    genlib_map:compact(ParsedParams#{
                        id => ID
                    }),
                    add_meta_to_ctx([], Params, EntityCtx)
                ))
            end)
        end,
    do(fun () -> unwrap(create_entity(p2p_template, Params, CreateFun, Context)) end).

-spec get_p2p_template(params(), ctx()) -> result(map(),
    p2p_template_machine:unknown_p2p_template_error()
).
get_p2p_template(ID, Context) ->
    do(fun () ->
        State = get_state(p2p_template, ID, Context),
        to_swag(p2p_template, State)
    end).

-spec block_p2p_template(params(), ctx()) ->
    ok | {error,
    {p2p_template, unauthorized} |
    p2p_template_machine:unknown_p2p_template_error()
}.
block_p2p_template(ID, Context) ->
    do(fun () ->
        _ = check_resource(p2p_template, ID, Context),
        p2p_template_machine:set_blocking(ID, blocked)
    end).

-spec issue_p2p_template_access_token(params(), binary(), ctx()) ->
    {ok, binary()} |
    {error,
        expired |
        {p2p_template, unauthorized} |
        p2p_template_machine:unknown_p2p_template_error()
}.
issue_p2p_template_access_token(ID, Expiration, Context) ->
    do(fun () ->
        _ = check_resource(p2p_template, ID, Context),
        Data = #{<<"expiration">> => Expiration},
        unwrap(wapi_backend_utils:issue_grant_token({p2p_templates, ID, Data}, Expiration, Context))
    end).

-spec issue_p2p_transfer_ticket(params(), binary(), ctx()) ->
    {ok, {binary(), binary()}} |
    {error,
        expired |
        p2p_template_machine:unknown_p2p_template_error()
}.
issue_p2p_transfer_ticket(ID, Expiration0, Context = #{woody_context := WoodyCtx}) ->
    do(fun () ->
        {_, _, Claims} = wapi_handler_utils:get_auth_context(Context),
        AccessData = maps:get(<<"data">>, Claims),
        AccessExpiration = maps:get(<<"expiration">>, AccessData),
        PartyID = wapi_handler_utils:get_owner(Context),
        Key  = bender_client:get_idempotent_key(<<"issue_p2p_transfer_ticket">>, ticket, PartyID, undefined),
        {ok, {TransferID, _}} = bender_client:gen_snowflake(Key, 0, WoodyCtx),
        Data = #{<<"transferID">> => TransferID},
        Expiration1 = choose_token_expiration(Expiration0, AccessExpiration),
        case wapi_backend_utils:issue_grant_token({p2p_template_transfers, ID, Data}, Expiration1, Context) of
            {ok, Token} ->
                {Token, Expiration1};
            {error, Error} ->
                throw(Error)
        end
    end).

-spec create_p2p_transfer_with_template(id(), params(), ctx()) -> result(map(),
    p2p_template_machine:unknown_p2p_template_error() |
    p2p_transfer:create_error() |
    {external_id_conflict, id(), external_id()} |
    {invalid_resource_token, _} |
    {quote, token_expired} |
    {token,
        {unsupported_version, integer() | undefined} |
        {not_verified, invalid_signature} |
        {not_verified, identity_mismatch}
    }
).
create_p2p_transfer_with_template(ID, Params, Context = #{woody_context := WoodyCtx}) ->
    do(fun () ->
        {_, _, Claims} = wapi_handler_utils:get_auth_context(Context),
        Data = maps:get(<<"data">>, Claims),
        TransferID = maps:get(<<"transferID">>, Data),
        PartyID = wapi_handler_utils:get_owner(Context),
        Hash = erlang:phash2(Params),
        IdempotentKey = wapi_backend_utils:get_idempotent_key(p2p_transfer_with_template, PartyID, TransferID),
        case bender_client:gen_constant(IdempotentKey, TransferID, Hash, WoodyCtx) of
            {ok, {TransferID, _}} ->
                ParsedParams = unwrap(maybe_add_p2p_template_quote_token(
                    ID, from_swag(create_p2p_with_template_params, Params)
                )),
                SenderResource = unwrap(construct_resource(maps:get(sender, ParsedParams))),
                ReceiverResource = unwrap(construct_resource(maps:get(receiver, ParsedParams))),
                RawSenderResource = {raw, #{
                    resource_params => SenderResource,
                    contact_info => maps:get(contact_info, ParsedParams)
                }},
                RawReceiverResource = {raw, #{resource_params => ReceiverResource, contact_info => #{}}},
                Result = p2p_template_machine:create_transfer(ID, ParsedParams#{
                    id => TransferID,
                    sender => RawSenderResource,
                    receiver => RawReceiverResource,
                    context => make_ctx(Context)
                }),
                unwrap(handle_create_entity_result(Result, p2p_transfer, TransferID, Context));
            {error, {external_id_conflict, {ConflictID, _}}} ->
                throw({external_id_conflict, ConflictID, TransferID})
        end
    end).

-spec quote_p2p_transfer_with_template(id(), params(), ctx()) -> result(map(),
    p2p_template_machine:unknown_p2p_template_error() |
    {invalid_resource_token, _} |
    p2p_quote:get_quote_error()
).
quote_p2p_transfer_with_template(ID, Params, Context) ->
    do(fun () ->
        #{
            sender := Sender,
            receiver := Receiver,
            body := Body
        } = from_swag(quote_p2p_with_template_params, Params),
        PartyID = wapi_handler_utils:get_owner(Context),
        SenderResource = unwrap(construct_resource(Sender)),
        ReceiverResource = unwrap(construct_resource(Receiver)),
        Quote = unwrap(p2p_template_machine:get_quote(ID, #{
            body => Body,
            sender => SenderResource,
            receiver => ReceiverResource
        })),
        Token = create_p2p_quote_token(Quote, PartyID),
        ExpiresOn = p2p_quote:expires_on(Quote),
        SurplusCash = get_p2p_quote_surplus(Quote),
        to_swag(p2p_transfer_quote, {SurplusCash, Token, ExpiresOn})
    end).

%% W2W

-spec create_w2w_transfer(params(), ctx()) -> result(map(), w2w_transfer:create_error()).
create_w2w_transfer(Params = #{<<"sender">> := WalletFromID}, Context) ->
    _ = check_resource(wallet, WalletFromID, Context),
    CreateFun =
        fun(ID, EntityCtx) ->
            ParsedParams = from_swag(create_w2w_params, Params),
            w2w_transfer_machine:create(
                genlib_map:compact(ParsedParams#{id => ID}),
                EntityCtx
            )
        end,
    do(fun () -> unwrap(create_entity(w2w_transfer, Params, CreateFun, Context)) end).

-spec get_w2w_transfer(params(), ctx()) -> result(map(),
    {w2w_transfer, unauthorized} |
    {w2w_transfer, {unknown_w2w_transfer, binary()}}
).
get_w2w_transfer(ID, Context) ->
    do(fun () ->
        State = get_state(w2w_transfer, ID, Context),
        to_swag(w2w_transfer, State)
    end).

%% Internal functions

choose_token_expiration(TicketExpiration, AccessExpiration) ->
    TicketMs = woody_deadline:to_unixtime_ms(woody_deadline:from_binary(TicketExpiration)),
    AccessMs = woody_deadline:to_unixtime_ms(woody_deadline:from_binary(AccessExpiration)),
    case TicketMs > AccessMs of
        true ->
            AccessExpiration;
        false ->
            TicketExpiration
    end.

construct_resource(#{<<"type">> := Type, <<"token">> := Token} = Resource)
when Type =:= <<"BankCardDestinationResource">> ->
    case wapi_crypto:decrypt_bankcard_token(Token) of
        unrecognized ->
            {ok, from_swag(destination_resource, Resource)};
        {ok, BankCard} ->
            {ok, {bank_card, encode_bank_card(BankCard)}};
        {error, {decryption_failed, _} = Error} ->
            logger:warning("~s token decryption failed: ~p", [Type, Error]),
            {error, {invalid_resource_token, Type}}
    end;
construct_resource(#{<<"type">> := Type, <<"token">> := Token, <<"authData">> := AuthData})
when   Type =:= <<"BankCardSenderResourceParams">>  ->
    case wapi_crypto:decrypt_bankcard_token(Token) of
        {ok, BankCard} ->
            {ok, encode_resource_bank_card(BankCard, AuthData)};
        unrecognized ->
            logger:warning("~s token unrecognized", [Type]),
            {error, {invalid_resource_token, Type}};
        {error, {decryption_failed, _} = Error} ->
            logger:warning("~s token decryption failed: ~p", [Type, Error]),
            {error, {invalid_resource_token, Type}}
    end;
construct_resource(#{<<"type">> := Type, <<"token">> := Token})
when   Type =:= <<"BankCardSenderResource">>
orelse Type =:= <<"BankCardReceiverResource">>
orelse Type =:= <<"BankCardReceiverResourceParams">> ->
    case wapi_crypto:decrypt_bankcard_token(Token) of
        {ok, BankCard} ->
            {ok, {bank_card, encode_bank_card(BankCard)}};
        unrecognized ->
            logger:warning("~s token unrecognized", [Type]),
            {error, {invalid_resource_token, Type}};
        {error, {decryption_failed, _} = Error} ->
            logger:warning("~s token decryption failed: ~p", [Type, Error]),
            {error, {invalid_resource_token, Type}}
    end;
construct_resource(#{<<"type">> := Type, <<"id">> := CryptoWalletID} = Resource)
when Type =:= <<"CryptoWalletDestinationResource">> ->
    {ok, {crypto_wallet, #{crypto_wallet => genlib_map:compact(#{
        id       => CryptoWalletID,
        currency => from_swag(crypto_wallet_currency, Resource)
    })}}}.

encode_resource_bank_card(BankCard, AuthData) ->
    EncodedBankCard = encode_bank_card(BankCard),
    {bank_card, EncodedBankCard#{auth_data => {session, #{session_id => AuthData}}}}.

encode_bank_card(BankCard) ->
    #{
        bank_card => genlib_map:compact(#{
            token           => BankCard#'BankCard'.token,
            bin             => BankCard#'BankCard'.bin,
            masked_pan      => BankCard#'BankCard'.masked_pan,
            cardholder_name => BankCard#'BankCard'.cardholder_name,
            %% ExpDate is optional in swag_wallets 'StoreBankCard'. But some adapters waiting exp_date.
            %% Add error, somethink like BankCardReject.exp_date_required
            exp_date        => encode_exp_date(BankCard#'BankCard'.exp_date)
        })
    }.

encode_exp_date(undefined) ->
    undefined;
encode_exp_date(ExpDate) ->
    #'BankCardExpDate'{
        month = Month,
        year = Year
    } = ExpDate,
    {Month, Year}.

encode_webhook_id(WebhookID) ->
    try
        binary_to_integer(WebhookID)
    catch
        error:badarg ->
            throw(notfound)
    end.

maybe_check_quote_token(Params = #{<<"quoteToken">> := QuoteToken}, Context) ->
    {ok, {_, _, Data}} = uac_authorizer_jwt:verify(QuoteToken, #{}),
    {ThriftQuote, WalletID, DestinationID, PartyID} = unwrap(quote, wapi_withdrawal_quote:decode_token_payload(Data)),
    Quote = ff_withdrawal_codec:unmarshal(quote, ThriftQuote),
    unwrap(quote_invalid_party,
        valid(
            PartyID,
            wapi_handler_utils:get_owner(Context)
    )),
    unwrap(quote_invalid_wallet,
        valid(
            WalletID,
            maps:get(<<"wallet">>, Params)
    )),
    check_quote_destination(
        DestinationID,
        maps:get(<<"destination">>, Params)
    ),
    check_quote_body(maps:get(cash_from, Quote), from_swag(body, maps:get(<<"body">>, Params))),
    {ok, Quote};
maybe_check_quote_token(_Params, _Context) ->
    {ok, undefined}.

check_quote_body(CashFrom, CashFrom) ->
    ok;
check_quote_body(_, CashFrom) ->
    throw({quote, {invalid_body, CashFrom}}).

check_quote_destination(undefined, _DestinationID) ->
    ok;
check_quote_destination(DestinationID, DestinationID) ->
    ok;
check_quote_destination(_, DestinationID) ->
    throw({quote, {invalid_destination, DestinationID}}).

create_quote_token(Quote, WalletID, DestinationID, PartyID) ->
    ThriftQuote = ff_withdrawal_codec:marshal(quote, Quote),
    Payload = wapi_withdrawal_quote:create_token_payload(ThriftQuote, WalletID, DestinationID, PartyID),
    {ok, Token} = issue_quote_token(PartyID, Payload),
    Token.

create_p2p_quote_token(Quote, PartyID) ->
    Payload = wapi_p2p_quote:create_token_payload(Quote, PartyID),
    {ok, Token} = issue_quote_token(PartyID, Payload),
    Token.

verify_p2p_quote_token(Token) ->
    case uac_authorizer_jwt:verify(Token, #{}) of
        {ok, {_, _, VerifiedToken}} ->
            {ok, VerifiedToken};
        {error, Error} ->
            {error, {token, {not_verified, Error}}}
    end.

authorize_p2p_quote_token(Quote, IdentityID) ->
    case p2p_quote:identity_id(Quote) of
        QuoteIdentityID when QuoteIdentityID =:= IdentityID ->
            ok;
        _OtherQuoteIdentityID ->
            {error, {token, {not_verified, identity_mismatch}}}
    end.

maybe_add_p2p_template_quote_token(_ID, #{quote_token := undefined} = Params) ->
    {ok, Params};
maybe_add_p2p_template_quote_token(ID, #{quote_token := QuoteToken} = Params) ->
    do(fun() ->
        VerifiedToken = unwrap(verify_p2p_quote_token(QuoteToken)),
        Quote = unwrap(quote, wapi_p2p_quote:decode_token_payload(VerifiedToken)),
        Machine = unwrap(p2p_template_machine:get(ID)),
        State = p2p_template_machine:p2p_template(Machine),
        ok = unwrap(authorize_p2p_quote_token(Quote, p2p_template:identity_id(State))),
        Params#{quote => Quote}
    end).

maybe_add_p2p_quote_token(#{quote_token := undefined} = Params) ->
    {ok, Params};
maybe_add_p2p_quote_token(#{quote_token := QuoteToken, identity_id := IdentityID} = Params) ->
    do(fun() ->
        VerifiedToken = unwrap(verify_p2p_quote_token(QuoteToken)),
        Quote = unwrap(quote, wapi_p2p_quote:decode_token_payload(VerifiedToken)),
        ok = unwrap(authorize_p2p_quote_token(Quote, IdentityID)),
        Params#{quote => Quote}
    end).

max_event_id(NewEventID, OldEventID) when is_integer(NewEventID) andalso is_integer(OldEventID) ->
    erlang:max(NewEventID, OldEventID);
max_event_id(NewEventID, OldEventID) ->
    genlib:define(NewEventID, OldEventID).

create_p2p_transfer_events_continuation_token(#{
    p2p_transfer_event_id := P2PTransferEventID,
    p2p_session_event_id := P2PSessionEventID
}, Context) ->
    DecodedToken = genlib_map:compact(#{
        <<"version">>               => 1,
        <<"p2p_transfer_event_id">> => P2PTransferEventID,
        <<"p2p_session_event_id">>  => P2PSessionEventID
    }),
    PartyID = wapi_handler_utils:get_owner(Context),
    {ok, SignedToken} = issue_quote_token(PartyID, DecodedToken),
    SignedToken.

prepare_p2p_transfer_event_continuation_token(_, undefined) ->
    {ok, #{}};
prepare_p2p_transfer_event_continuation_token(PartyID, CT) ->
    do(fun() ->
        VerifiedCT = unwrap(verify_p2p_transfer_event_continuation_token(PartyID, CT)),
        DecodedCT = unwrap(decode_p2p_transfer_event_continuation_token(VerifiedCT)),
        DecodedCT
    end).

verify_p2p_transfer_event_continuation_token(PartyID, CT) ->
    do(fun() ->
        case uac_authorizer_jwt:verify(CT, #{}) of
            {ok, {_, PartyID, VerifiedToken}} ->
                VerifiedToken;
            {error, Error} ->
                {error, {token, {not_verified, Error}}};
            _ ->
                {error, {token, {not_verified, wrong_party_id}}}
        end
    end).

decode_p2p_transfer_event_continuation_token(CT) ->
    do(fun() ->
        case CT of
            #{<<"version">> := 1} ->
                DecodedToken = #{
                    p2p_transfer_event_id => maps:get(<<"p2p_transfer_event_id">>, CT, undefined),
                    p2p_session_event_id => maps:get(<<"p2p_session_event_id">>, CT, undefined)
                },
                DecodedToken;
            #{<<"version">> := UnsupportedVersion} when is_integer(UnsupportedVersion) ->
                {error, {token, {unsupported_version, UnsupportedVersion}}}
        end
    end).

-spec mix_events(list(p2p_transfer_machine:events() | p2p_session_machine:events())) ->
    [{id(), ff_machine:timestamped_event(p2p_transfer:event() | p2p_session:event())}].
mix_events(EventsList) ->
    AppendedEvents = lists:append(EventsList),
    sort_events_by_timestamp(AppendedEvents).

sort_events_by_timestamp(Events) ->
    lists:keysort(2, Events).

filter_identity_challenge_status(Filter, Status) ->
    maps:get(<<"status">>, to_swag(challenge_status, Status)) =:= Filter.

maybe_get_session_events(TransferID, Limit, P2PSessionEventID, Context) ->
    do(fun() ->
        P2PTransfer = p2p_transfer_machine:p2p_transfer(get_state(p2p_transfer, TransferID, Context)),
        Filter = fun session_events_filter/1,
        case p2p_transfer:session_id(P2PTransfer) of
            undefined ->
                {[], undefined};
            SessionID ->
                unwrap(get_events_unauthorized({p2p_session, event}, SessionID, Limit, P2PSessionEventID, Filter))
        end
    end).

maybe_get_transfer_events(TransferID, Limit, P2PTransferEventID, Context) ->
    Filter = fun transfer_events_filter/1,
    get_events({p2p_transfer, event}, TransferID, Limit, P2PTransferEventID, Filter, Context).

session_events_filter({_ID, {ev, _Timestamp, {user_interaction, #{payload := Payload}}}})
    when Payload =/= {status_changed, pending}
->
    true;
session_events_filter(_) ->
    false.

transfer_events_filter({_ID, {ev, _Timestamp, {EventType, _}}}) when EventType =:= status_changed ->
    true;
transfer_events_filter(_) ->
    false.

get_swag_event(Type, ResourceId, EventId, Filter, Context) ->
    case get_swag_events(Type, ResourceId, 1, EventId - 1, Filter, Context) of
        {ok, [Event]}      -> {ok, Event};
        {ok, []}           -> {error, {event, notfound}};
        Error = {error, _} -> Error
    end.

get_swag_events(Type, ResourceId, Limit, Cursor, Filter, Context) ->
    do(fun() ->
        {Events, _LastEventID} = unwrap(get_events(Type, ResourceId, Limit, Cursor, Filter, Context)),
        to_swag(
            {list, get_event_type(Type)},
            Events
        )
    end).

get_events_unauthorized(Type, ResourceId, Limit, Cursor, Filter) ->
    do(fun() -> collect_events(get_collector(Type, ResourceId), Filter, Cursor, Limit) end).

get_events(Type = {Resource, _}, ResourceId, Limit, Cursor, Filter, Context) ->
    do(fun() ->
        _ = check_resource(Resource, ResourceId, Context),
        collect_events(get_collector(Type, ResourceId), Filter, Cursor, Limit)
    end).

get_event_type({identity, challenge_event}) -> identity_challenge_event;
get_event_type({withdrawal, event})         -> withdrawal_event.

get_collector({identity, challenge_event}, Id) ->
    fun(C, L) -> unwrap(ff_identity_machine:events(Id, {C, L})) end;
get_collector({withdrawal, event}, Id) ->
    fun(C, L) -> unwrap(ff_withdrawal_machine:events(Id, {C, L})) end;
get_collector({p2p_transfer, event}, Id) ->
    fun(C, L) -> unwrap(p2p_transfer_machine:events(Id, {C, L})) end;
get_collector({p2p_session, event}, Id) ->
    fun(C, L) -> unwrap(p2p_session_machine:events(Id, {C, L})) end.

collect_events(Collector, Filter, Cursor, Limit) ->
    collect_events(Collector, Filter, Cursor, Limit, {[], undefined}).

collect_events(Collector, Filter, Cursor, Limit, {AccEvents, LastEventID}) when Limit =:= undefined ->
    case Collector(Cursor, Limit) of
        [] ->
            {AccEvents, LastEventID};
        Events1 ->
            {_, Events2} = filter_events(Filter, Events1),
            {NewLastEventID, _} = lists:last(Events1),
            {AccEvents ++ Events2, NewLastEventID}
    end;
collect_events(Collector, Filter, Cursor, Limit, {AccEvents, LastEventID}) ->
    case Collector(Cursor, Limit) of
        [] ->
            {AccEvents, LastEventID};
        Events1 ->
            {CursorNext, Events2} = filter_events(Filter, Events1),
            {NewLastEventID, _} = lists:last(Events1),
            NewAcc = {AccEvents ++ Events2, NewLastEventID},
            collect_events(Collector, Filter, CursorNext, Limit - length(Events2), NewAcc)
    end.

filter_events(Filter, Events) ->
    {Cursor, _} = lists:last(Events),
    {Cursor, lists:filtermap(Filter, Events)}.

enrich_proofs(Proofs, Context) ->
    [enrich_proof(P, Context) || P <- Proofs].

enrich_proof({_, Token}, Context) ->
    wapi_privdoc_backend:get_proof(Token, Context).

get_state(Resource, Id, Context) ->
    State = unwrap(Resource, do_get_state(Resource, Id)),
    ok    = unwrap(Resource, check_resource_access(Context, State)),
    State.

do_get_state(identity,     Id) -> ff_identity_machine:get(Id);
do_get_state(wallet,       Id) -> ff_wallet_machine:get(Id);
do_get_state(destination,  Id) -> ff_destination:get_machine(Id);
do_get_state(withdrawal,   Id) -> ff_withdrawal_machine:get(Id);
do_get_state(p2p_transfer, Id) -> p2p_transfer_machine:get(Id);
do_get_state(p2p_template, Id) -> p2p_template_machine:get(Id);
do_get_state(w2w_transfer, Id) -> w2w_transfer_machine:get(Id).

check_resource(Resource, Id, Context) ->
    _ = get_state(Resource, Id, Context),
    ok.

make_ctx(Context) ->
    #{?CTX_NS => #{<<"owner">> => wapi_handler_utils:get_owner(Context)}}.

add_meta_to_ctx(WapiKeys, Params, Context = #{?CTX_NS := Ctx}) ->
    Context#{?CTX_NS => maps:merge(
        Ctx,
        maps:with(WapiKeys, Params)
    )}.

add_to_ctx(Key, Value, Context = #{?CTX_NS := Ctx}) ->
    Context#{?CTX_NS => Ctx#{Key => Value}}.

get_ctx(State) ->
    unwrap(ff_entity_context:get(?CTX_NS, ff_machine:ctx(State))).

get_resource_owner(State) ->
    maps:get(<<"owner">>, get_ctx(State)).

is_resource_owner(HandlerCtx, State) ->
    wapi_handler_utils:get_owner(HandlerCtx) =:= get_resource_owner(State).

check_resource_access(HandlerCtx, State) ->
    check_resource_access(is_resource_owner(HandlerCtx, State)).

check_resource_access(true)  -> ok;
check_resource_access(false) -> {error, unauthorized}.

create_entity(Type, Params, CreateFun, Context) ->
    ExternalID = maps:get(<<"externalID">>, Params, undefined),
    Hash       = erlang:phash2(Params),
    case gen_id(Type, ExternalID, Hash, Context) of
        {ok, ID} ->
            Result = CreateFun(ID, add_to_ctx(?PARAMS_HASH, Hash, make_ctx(Context))),
            handle_create_entity_result(Result, Type, ID, Context);
        {error, {external_id_conflict, ID}} ->
            {error, {external_id_conflict, ID, ExternalID}}
    end.

handle_create_entity_result(Result, Type, ID, Context) when
    Result =:= ok;
    Result =:= {error, exists}
->
    St = get_state(Type, ID, Context),
    do(fun() -> to_swag(Type, St) end);
handle_create_entity_result({error, E}, _Type, _ID, _Context) ->
    throw(E).

with_party(Context, Fun) ->
    try Fun()
    catch
        error:#'payproc_PartyNotFound'{} ->
            ok = create_party(Context),
            Fun()
    end.

create_party(Context) ->
    _ = ff_party:create(
        wapi_handler_utils:get_owner(Context),
        #{email => unwrap(get_email(wapi_handler_utils:get_auth_context(Context)))}
    ),
    ok.

get_email(AuthContext) ->
    case uac_authorizer_jwt:get_claim(<<"email">>, AuthContext, undefined) of
        undefined -> {error, {email, notfound}};
        Email     -> {ok, Email}
    end.

-spec not_implemented() -> no_return().
not_implemented() ->
    wapi_handler_utils:throw_not_implemented().

do(Fun) ->
    ff_pipeline:do(Fun).

-spec unwrap
    (ok)         -> ok;
    ({ok, V})    -> V;
    ({error, _E}) -> no_return().
unwrap(Res) ->
    ff_pipeline:unwrap(Res).

-spec unwrap
    (_Tag, ok)         -> ok;
    (_Tag, {ok, V})    -> V;
    (_Tag, {error, _E}) -> no_return().
unwrap(Tag, Res) ->
    ff_pipeline:unwrap(Tag, Res).

valid(Val1, Val2) ->
    ff_pipeline:valid(Val1, Val2).

get_contract_id_from_identity(IdentityID, Context) ->
    State = get_state(identity, IdentityID, Context),
    ff_identity:contract(ff_machine:model(State)).

%% ID Gen

gen_id(Type, ExternalID, Hash, Context) ->
    PartyID = wapi_handler_utils:get_owner(Context),
    IdempotentKey = wapi_backend_utils:get_idempotent_key(Type, PartyID, ExternalID),
    gen_id_by_type(Type, IdempotentKey, Hash, Context).

%@TODO: Bring back later
%gen_id_by_type(withdrawal = Type, IdempotentKey, Hash, Context) ->
%    gen_snowflake_id(Type, IdempotentKey, Hash, Context);
gen_id_by_type(Type, IdempotentKey, Hash, Context) ->
    gen_sequence_id(Type, IdempotentKey, Hash, Context).

%@TODO: Bring back later
%gen_snowflake_id(_Type, IdempotentKey, Hash, #{woody_context := WoodyCtx}) ->
%    bender_client:gen_snowflake(IdempotentKey, Hash, WoodyCtx).

gen_sequence_id(Type, IdempotentKey, Hash, #{woody_context := WoodyCtx}) ->
    BinType = atom_to_binary(Type, utf8),
    case bender_client:gen_sequence(IdempotentKey, BinType, Hash, WoodyCtx) of
        {ok, {ID, _IntegerID}} -> {ok, ID}; % No need for IntegerID at this project so far
        {error, {external_id_conflict, {ID, _IntegerID}}} -> {error, {external_id_conflict, ID}}
    end.

create_report_request(#{
    party_id     := PartyID,
    contract_id  := ContractID,
    from_time    := FromTime,
    to_time      := ToTime
}) ->
    #'ff_reports_ReportRequest'{
        party_id    = PartyID,
        contract_id = ContractID,
        time_range  = #'ff_reports_ReportTimeRange'{
            from_time = FromTime,
            to_time   = ToTime
        }
    }.

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
decode_withdrawal_stat_status({failed, #fistfulstat_WithdrawalFailed{failure = Failure}}) ->
    #{
        <<"status">> => <<"Failed">>,
        <<"failure">> => to_swag(stat_status_failure, Failure)
    }.

decode_deposit_stat_status({pending, #fistfulstat_DepositPending{}}) ->
    #{<<"status">> => <<"Pending">>};
decode_deposit_stat_status({succeeded, #fistfulstat_DepositSucceeded{}}) ->
    #{<<"status">> => <<"Succeeded">>};
decode_deposit_stat_status({failed, #fistfulstat_DepositFailed{failure = Failure}}) ->
    #{
        <<"status">> => <<"Failed">>,
        <<"failure">> => to_swag(stat_status_failure, Failure)
    }.

%% Marshalling

add_external_id(Params, #{?EXTERNAL_ID := Tag}) ->
    Params#{external_id => Tag};
add_external_id(Params, _) ->
    Params.

-type swag_term() ::
    #{binary() => swag_term()} |
    [swag_term()]              |
    number()                   |
    binary()                   |
    boolean()                  .

-spec from_swag(_Type, swag_term()) ->
    _Term.

from_swag(create_quote_params, Params) ->
    genlib_map:compact(add_external_id(#{
        wallet_id       => maps:get(<<"walletID">>, Params),
        currency_from   => from_swag(currency, maps:get(<<"currencyFrom">>, Params)),
        currency_to     => from_swag(currency, maps:get(<<"currencyTo">>, Params)),
        body            => from_swag(body, maps:get(<<"cash">>, Params)),
        destination_id  => maps:get(<<"destinationID">>, Params, undefined)
    }, Params));
from_swag(identity_params, Params) ->
    genlib_map:compact(add_external_id(#{
        provider => maps:get(<<"provider">>, Params),
        class    => maps:get(<<"class">>   , Params),
        metadata => maps:get(<<"metadata">>, Params, undefined)
    }, Params));
from_swag(identity_challenge_params, Params) ->
    #{
       class  => maps:get(<<"type">>, Params),
       proofs => from_swag(proofs, maps:get(<<"proofs">>, Params))
    };
from_swag(proofs, Proofs) ->
    from_swag({list, proof}, Proofs);
from_swag(proof, #{<<"token">> := WapiToken}) ->
    try
        #{<<"type">> := Type, <<"token">> := Token} = wapi_utils:base64url_to_map(WapiToken),
        {from_swag(proof_type, Type), Token}
    catch
        error:badarg ->
            wapi_handler:throw_result(wapi_handler_utils:reply_error(
                422,
                wapi_handler_utils:get_error_msg(io_lib:format("Invalid proof token: ~p", [WapiToken]))
            ))
    end;
from_swag(proof_type, <<"RUSDomesticPassport">>) ->
    rus_domestic_passport;
from_swag(proof_type, <<"RUSRetireeInsuranceCertificate">>) ->
    rus_retiree_insurance_cert;

from_swag(wallet_params, Params) ->
    genlib_map:compact(add_external_id(#{
        identity => maps:get(<<"identity">>, Params),
        currency => maps:get(<<"currency">>, Params),
        name     => maps:get(<<"name">>    , Params),
        metadata => maps:get(<<"metadata">>, Params, undefined)
    }, Params));
from_swag(destination_params, Params) ->
    genlib_map:compact(add_external_id(#{
        identity => maps:get(<<"identity">>, Params),
        currency => maps:get(<<"currency">>, Params),
        name     => maps:get(<<"name">>    , Params),
        resource => maps:get(<<"resource">>, Params),
        metadata => maps:get(<<"metadata">>, Params, undefined)
    }, Params));
%% TODO delete this code, after add encrypted token
from_swag(destination_resource, #{
    <<"type">> := <<"BankCardDestinationResource">>,
    <<"token">> := WapiToken
}) ->
    BankCard = wapi_utils:base64url_to_map(WapiToken),
    {bank_card, #{bank_card => #{
        token          => maps:get(<<"token">>, BankCard),
        bin            => maps:get(<<"bin">>, BankCard),
        masked_pan     => maps:get(<<"lastDigits">>, BankCard)
    }}};
from_swag(destination_resource, Resource = #{
    <<"type">>     := <<"CryptoWalletDestinationResource">>,
    <<"id">>       := CryptoWalletID,
    <<"currency">> := CryptoWalletCurrency
}) ->
    Tag = maps:get(<<"tag">>, Resource, undefined),
    {crypto_wallet, #{crypto_wallet => genlib_map:compact(#{
        id       => CryptoWalletID,
        currency => from_swag(crypto_wallet_currency, CryptoWalletCurrency),
        tag      => Tag
    })}};
from_swag(quote_p2p_params, Params) ->
    add_external_id(#{
        sender      => maps:get(<<"sender">>, Params),
        receiver    => maps:get(<<"receiver">>, Params),
        identity_id => maps:get(<<"identityID">>, Params),
        body        => from_swag(body, maps:get(<<"body">>, Params))
    }, Params);
from_swag(quote_p2p_with_template_params, Params) ->
    add_external_id(#{
        sender      => maps:get(<<"sender">>, Params),
        receiver    => maps:get(<<"receiver">>, Params),
        body        => from_swag(body, maps:get(<<"body">>, Params))
    }, Params);

from_swag(create_p2p_params, Params) ->
    add_external_id(#{
        sender => maps:get(<<"sender">>, Params),
        receiver => maps:get(<<"receiver">>, Params),
        identity_id => maps:get(<<"identityID">>, Params),
        body => from_swag(body, maps:get(<<"body">>, Params)),
        quote_token => maps:get(<<"quoteToken">>, Params, undefined),
        metadata => maps:get(<<"metadata">>, Params, #{}),
        contact_info => from_swag(contact_info, maps:get(<<"contactInfo">>, Params))
    }, Params);

from_swag(create_p2p_with_template_params, Params) ->
    #{
        sender => maps:get(<<"sender">>, Params),
        receiver => maps:get(<<"receiver">>, Params),
        body => from_swag(body, maps:get(<<"body">>, Params)),
        quote_token => maps:get(<<"quoteToken">>, Params, undefined),
        metadata => maps:get(<<"metadata">>, Params, #{}),
        contact_info => from_swag(contact_info, maps:get(<<"contactInfo">>, Params))
    };

from_swag(p2p_template_create_params, Params) ->
    add_external_id(#{
        identity_id => maps:get(<<"identityID">>, Params),
        details => from_swag(p2p_template_details, maps:get(<<"details">>, Params))
    }, Params);

from_swag(p2p_template_details, Details) ->
    genlib_map:compact(#{
        body => from_swag(p2p_template_body, maps:get(<<"body">>, Details)),
        metadata => maybe_from_swag(p2p_template_metadata, maps:get(<<"metadata">>, Details, undefined))
    });

from_swag(p2p_template_body, #{<<"value">> := Body}) ->
    #{value => genlib_map:compact(#{
        currency => from_swag(currency, maps:get(<<"currency">>, Body)),
        amount => maybe_from_swag(amount, maps:get(<<"amount">>, Body, undefined))
    })};

from_swag(p2p_template_metadata, #{<<"defaultMetadata">> := Metadata}) ->
    #{value => Metadata};

from_swag(create_w2w_params, Params) ->
    genlib_map:compact(add_external_id(#{
        wallet_from_id => maps:get(<<"sender">>, Params),
        wallet_to_id => maps:get(<<"receiver">>, Params),
        body => from_swag(body, maps:get(<<"body">>, Params)),
        metadata => maps:get(<<"metadata">>, Params, undefined)
    }, Params));

from_swag(destination_resource, Resource = #{
    <<"type">>     := <<"CryptoWalletDestinationResource">>,
    <<"id">>       := CryptoWalletID
}) ->
    {crypto_wallet, genlib_map:compact(#{
        id       => CryptoWalletID,
        currency => from_swag(crypto_wallet_currency, Resource)
    })};

from_swag(crypto_wallet_currency, #{<<"currency">> := <<"Ripple">>} = Resource) ->
    Currency = from_swag(crypto_wallet_currency_name, <<"Ripple">>),
    Data = genlib_map:compact(#{tag => maps:get(<<"tag">>, Resource, undefined)}),
    {Currency, Data};
from_swag(crypto_wallet_currency, #{<<"currency">> := Currency}) ->
    {from_swag(crypto_wallet_currency_name, Currency), #{}};

from_swag(crypto_wallet_currency_name, <<"Bitcoin">>)     -> bitcoin;
from_swag(crypto_wallet_currency_name, <<"Litecoin">>)    -> litecoin;
from_swag(crypto_wallet_currency_name, <<"BitcoinCash">>) -> bitcoin_cash;
from_swag(crypto_wallet_currency_name, <<"Ethereum">>)    -> ethereum;
from_swag(crypto_wallet_currency_name, <<"Zcash">>)       -> zcash;
from_swag(crypto_wallet_currency_name, <<"Ripple">>)      -> ripple;
from_swag(crypto_wallet_currency_name, <<"USDT">>)        -> usdt;

from_swag(withdrawal_params, Params) ->
    genlib_map:compact(add_external_id(#{
        wallet_id      => maps:get(<<"wallet">>     , Params),
        destination_id => maps:get(<<"destination">>, Params),
        body           => from_swag(body , maps:get(<<"body">>, Params)),
        metadata       => maps:get(<<"metadata">>, Params, undefined)
    }, Params));

from_swag(contact_info, ContactInfo) ->
    genlib_map:compact(#{
        phone_number => maps:get(<<"phoneNumber">>, ContactInfo, undefined),
        email => maps:get(<<"email">>, ContactInfo, undefined)
    });

%% TODO
%%  - remove this clause when we fix negative accounts and turn on validation in swag
from_swag(body, #{<<"amount">> := Amount}) when Amount < 0 ->
    wapi_handler:throw_result(wapi_handler_utils:reply_error(400, #{<<"errorType">> => <<"WrongSize">>}));
from_swag(body, Body) ->
    {genlib:to_int(maps:get(<<"amount">>, Body)), maps:get(<<"currency">>, Body)};
from_swag(amount, Amount) ->
    genlib:to_int(Amount);
from_swag(currency, V) ->
    V;
from_swag(residence, V) ->
    try erlang:binary_to_existing_atom(genlib_string:to_lower(V), latin1) catch
        error:badarg ->
            % TODO
            %  - Essentially this is incorrect, we should reply with 400 instead
            undefined
    end;
from_swag(webhook_params, #{
    <<"identityID">> := IdentityID,
    <<"scope">> := Scope,
    <<"url">> := URL
}) ->
    maps:merge(
        #{
            identity_id => IdentityID,
            url => URL
        },
        from_swag(webhook_scope, Scope)
    );
from_swag(webhook_scope, Topic = #{
    <<"topic">> := <<"WithdrawalsTopic">>,
    <<"eventTypes">> := EventList
}) ->
    WalletID = maps:get(<<"walletID">>, Topic, undefined),
    Scope = #webhooker_EventFilter{
        types = from_swag({set, webhook_withdrawal_event_types}, EventList)
    },
    genlib_map:compact(#{
        scope => Scope,
        wallet_id => WalletID
    });
from_swag(webhook_scope, #{
    <<"topic">> := <<"DestinationsTopic">>,
    <<"eventTypes">> := EventList
}) ->
    Scope = #webhooker_EventFilter{
        types = from_swag({set, webhook_destination_event_types}, EventList)
    },
    #{
        scope => Scope
    };
from_swag(webhook_withdrawal_event_types, <<"WithdrawalStarted">>) ->
    {withdrawal, {started, #webhooker_WithdrawalStarted{}}};
from_swag(webhook_withdrawal_event_types, <<"WithdrawalSucceeded">>) ->
    {withdrawal, {succeeded, #webhooker_WithdrawalSucceeded{}}};
from_swag(webhook_withdrawal_event_types, <<"WithdrawalFailed">>) ->
    {withdrawal, {failed, #webhooker_WithdrawalFailed{}}};

from_swag(webhook_destination_event_types, <<"DestinationCreated">>) ->
    {destination, {created, #webhooker_DestinationCreated{}}};
from_swag(webhook_destination_event_types, <<"DestinationUnauthorized">>) ->
    {destination, {unauthorized, #webhooker_DestinationUnauthorized{}}};
from_swag(webhook_destination_event_types, <<"DestinationAuthorized">>) ->
    {destination, {authorized, #webhooker_DestinationAuthorized{}}};

from_swag({list, Type}, List) ->
    lists:map(fun(V) -> from_swag(Type, V) end, List);
from_swag({set, Type}, List) ->
    ordsets:from_list(from_swag({list, Type}, List)).

maybe_from_swag(_T, undefined) ->
    undefined;
maybe_from_swag(T, V) ->
    from_swag(T, V).

-spec to_swag(_Type, _Value) ->
    swag_term() | undefined.

to_swag(_, undefined) ->
    undefined;
to_swag(providers, Providers) ->
    to_swag({list, provider}, Providers);
to_swag(provider, Provider) ->
    to_swag(map, #{
       <<"id">> => ff_provider:id(Provider),
       <<"name">> => ff_provider:name(Provider),
       <<"residences">> => to_swag({list, residence},
           ordsets:to_list(ff_provider:residences(Provider))
       )
     });
to_swag(residence, Residence) ->
    genlib_string:to_upper(genlib:to_binary(Residence));
to_swag(residence_object, V) ->
    to_swag(map, #{
        <<"id">>   => to_swag(residence, maps:get(id, V)),
        <<"name">> => maps:get(name, V),
        <<"flag">> => maps:get(flag, V, undefined)
    });
to_swag(identity_class, Class) ->
    to_swag(map, maps:with([id, name], Class));
to_swag(identity, State) ->
    Identity = ff_identity_machine:identity(State),
    WapiCtx  = get_ctx(State),
    to_swag(map, #{
        <<"id">>                 => ff_identity:id(Identity),
        <<"name">>               => maps:get(<<"name">>, WapiCtx),
        <<"createdAt">>          => to_swag(timestamp, ff_machine:created(State)),
        <<"provider">>           => ff_identity:provider(Identity),
        <<"class">>              => ff_identity:class(Identity),
        <<"level">>              => ff_identity:level(Identity),
        <<"effectiveChallenge">> => to_swag(identity_effective_challenge, ff_identity:effective_challenge(Identity)),
        <<"isBlocked">>          => to_swag(is_blocked, ff_identity:is_accessible(Identity)),
        <<"metadata">>           => ff_identity:metadata(Identity),
        ?EXTERNAL_ID             => ff_identity:external_id(Identity)
    });
to_swag(identity_effective_challenge, {ok, ChallegeId}) ->
    ChallegeId;
to_swag(identity_effective_challenge, {error, notfound}) ->
    undefined;
to_swag(identity_challenge, {ChallengeId, Challenge, Proofs}) ->
    ChallengeClass = ff_identity_challenge:class(Challenge),
    to_swag(map, maps:merge(#{
        <<"id">>            => ChallengeId,
        %% TODO add createdAt when it is available on the backend
        %% <<"createdAt">>     => _,
        <<"type">>          => ChallengeClass,
        <<"proofs">>        => Proofs
    }, to_swag(challenge_status, ff_identity_challenge:status(Challenge))));
to_swag(challenge_status, pending) ->
    #{<<"status">>  => <<"Pending">>};
to_swag(challenge_status, cancelled) ->
    #{<<"status">>  => <<"Cancelled">>};
to_swag(challenge_status, {completed, C = #{resolution := approved}}) ->
    to_swag(map, #{
        <<"status">>        => <<"Completed">>,
        <<"validUntil">>    => to_swag(timestamp, genlib_map:get(valid_until, C))
    });
to_swag(challenge_status, {completed, #{resolution := denied}}) ->
    to_swag(challenge_status, {failed, <<"Denied">>});
to_swag(challenge_status, {failed, Reason}) ->
    #{
        <<"status">>        => <<"Failed">>,
        <<"failureReason">> => to_swag(challenge_failure_reason, Reason)
    };
to_swag(challenge_failure_reason, Reason) ->
    genlib:to_binary(Reason);
to_swag(identity_challenge_event, {ID, Ts, V}) ->
    #{
        <<"eventID">>   => ID,
        <<"occuredAt">> => to_swag(timestamp, Ts),
        <<"changes">>   => [to_swag(identity_challenge_event_change, V)]
    };

to_swag(identity_challenge_event_change, {status_changed, S}) ->
    to_swag(map, maps:merge(
        #{<<"type">> => <<"IdentityChallengeStatusChanged">>},
        to_swag(challenge_status, S)
    ));

to_swag(p2p_transfer_events, {Events, ContinuationToken}) ->
    #{
        <<"continuationToken">> => ContinuationToken,
        <<"result">> => to_swag({list, p2p_transfer_event}, Events)
    };

to_swag(p2p_transfer_event, {_ID, {ev, Ts, V}}) ->
    #{
        <<"createdAt">> => to_swag(timestamp, Ts),
        <<"change">>    => to_swag(p2p_transfer_event_change, V)
    };

to_swag(p2p_transfer_event_change, {status_changed, Status}) ->
    ChangeType = #{
        <<"changeType">> => <<"P2PTransferStatusChanged">>
    },
    TransferChange = to_swag(p2p_transfer_status, Status),
    maps:merge(ChangeType, TransferChange);
to_swag(p2p_transfer_event_change, {user_interaction, #{
    id := ID,
    payload := Payload
}}) ->
    #{
        <<"changeType">> => <<"P2PTransferInteractionChanged">>,
        <<"userInteractionID">> => ID,
        <<"userInteractionChange">> => to_swag(p2p_transfer_user_interaction_change, Payload)
    };

to_swag(p2p_transfer_user_interaction_change, {created, #{
    version := 1,
    content := Content
}}) ->
    #{
        <<"changeType">> => <<"UserInteractionCreated">>,
        <<"userInteraction">> => to_swag(p2p_transfer_user_interaction, Content)
    };
to_swag(p2p_transfer_user_interaction_change, {status_changed, finished}) ->
    #{
        <<"changeType">> => <<"UserInteractionFinished">>
    };

to_swag(p2p_transfer_user_interaction, {redirect, #{
    content := Redirect
}}) ->
    #{
        <<"interactionType">> => <<"Redirect">>,
        <<"request">> => to_swag(browser_request, Redirect)
    };

to_swag(browser_request, {get, URI}) ->
    #{
        <<"requestType">> => <<"BrowserGetRequest">>,
        <<"uriTemplate">> => URI
    };
to_swag(browser_request, {post, URI, Form}) ->
    #{
        <<"requestType">> => <<"BrowserPostRequest">>,
        <<"uriTemplate">> => URI,
        <<"form">> => to_swag(user_interaction_form, Form)
    };

to_swag(user_interaction_form, Form) ->
    maps:fold(
        fun (Key, Template, AccIn) ->
            FormField = #{
                <<"key">> => Key,
                <<"template">> => Template
            },
            [FormField | AccIn]
        end,
        [], Form
    );

to_swag(wallet, State) ->
    Wallet = ff_wallet_machine:wallet(State),
    to_swag(map, #{
        <<"id">>         => ff_wallet:id(Wallet),
        <<"name">>       => ff_wallet:name(Wallet),
        <<"createdAt">>  => to_swag(timestamp, ff_machine:created(State)),
        <<"isBlocked">>  => to_swag(is_blocked, ff_wallet:is_accessible(Wallet)),
        <<"identity">>   => ff_wallet:identity(Wallet),
        <<"currency">>   => to_swag(currency, ff_wallet:currency(Wallet)),
        <<"metadata">>   => ff_wallet:metadata(Wallet),
        ?EXTERNAL_ID     => ff_wallet:external_id(Wallet)
    });
to_swag(wallet_account, {OwnAmount, AvailableAmount, Currency}) ->
    EncodedCurrency = to_swag(currency, Currency),
    #{
        <<"own">> => #{
            <<"amount">>   => OwnAmount,
            <<"currency">> => EncodedCurrency
        },
        <<"available">> => #{
            <<"amount">>   => AvailableAmount,
            <<"currency">> => EncodedCurrency
        }
    };
to_swag(destination, State) ->
    Destination = ff_destination:get(State),
    to_swag(map, maps:merge(
        #{
            <<"id">>         => ff_destination:id(Destination),
            <<"name">>       => ff_destination:name(Destination),
            <<"createdAt">>  => to_swag(timestamp, ff_machine:created(State)),
            <<"isBlocked">>  => to_swag(is_blocked, ff_destination:is_accessible(Destination)),
            <<"identity">>   => ff_destination:identity(Destination),
            <<"currency">>   => to_swag(currency, ff_destination:currency(Destination)),
            <<"resource">>   => to_swag(destination_resource, ff_destination:resource(Destination)),
            <<"metadata">>   => ff_destination:metadata(Destination),
            ?EXTERNAL_ID     => ff_destination:external_id(Destination)
        },
        to_swag(destination_status, ff_destination:status(Destination))
    ));
%% TODO: add validUntil when it is supported by the ff_destination
%% to_swag(destination_status, {authorized, Timeout}) ->
%%     #{
%%         <<"status">>     => <<"Authorized">>,
%%         <<"validUntil">> => to_swag(timestamp, Timeout)
%%     };
to_swag(destination_status, authorized) ->
    #{<<"status">> => <<"Authorized">>};
to_swag(destination_status, unauthorized) ->
    #{<<"status">> => <<"Unauthorized">>};
to_swag(destination_resource, {bank_card, #{bank_card := BankCard}}) ->
    to_swag(map, #{
        <<"type">>          => <<"BankCardDestinationResource">>,
        <<"token">>         => maps:get(token, BankCard),
        <<"bin">>           => genlib_map:get(bin, BankCard),
        <<"lastDigits">>    => to_swag(pan_last_digits, genlib_map:get(masked_pan, BankCard))
    });
to_swag(destination_resource, {crypto_wallet, #{crypto_wallet := CryptoWallet}}) ->
    to_swag(map, maps:merge(#{
        <<"type">>     => <<"CryptoWalletDestinationResource">>,
        <<"id">>       => maps:get(id, CryptoWallet)
    }, to_swag(crypto_wallet_currency, maps:get(currency, CryptoWallet))));
to_swag(sender_resource, {bank_card, #{bank_card := BankCard}}) ->
    to_swag(map, #{
        <<"type">>          => <<"BankCardSenderResource">>,
        <<"token">>         => maps:get(token, BankCard),
        <<"paymentSystem">> => genlib:to_binary(genlib_map:get(payment_system, BankCard)),
        <<"bin">>           => genlib_map:get(bin, BankCard),
        <<"lastDigits">>    => to_swag(pan_last_digits, genlib_map:get(masked_pan, BankCard))
    });
to_swag(receiver_resource, {bank_card, #{bank_card := BankCard}}) ->
    to_swag(map, #{
        <<"type">>          => <<"BankCardReceiverResource">>,
        <<"token">>         => maps:get(token, BankCard),
        <<"paymentSystem">> => genlib:to_binary(genlib_map:get(payment_system, BankCard)),
        <<"bin">>           => genlib_map:get(bin, BankCard),
        <<"lastDigits">>    => to_swag(pan_last_digits, genlib_map:get(masked_pan, BankCard))
    });

to_swag(pan_last_digits, MaskedPan) ->
    wapi_utils:get_last_pan_digits(MaskedPan);

to_swag(crypto_wallet_currency, {bitcoin, #{}})          -> #{<<"currency">> => <<"Bitcoin">>};
to_swag(crypto_wallet_currency, {litecoin, #{}})         -> #{<<"currency">> => <<"Litecoin">>};
to_swag(crypto_wallet_currency, {bitcoin_cash, #{}})     -> #{<<"currency">> => <<"BitcoinCash">>};
to_swag(crypto_wallet_currency, {ethereum, #{}})         -> #{<<"currency">> => <<"Ethereum">>};
to_swag(crypto_wallet_currency, {zcash, #{}})            -> #{<<"currency">> => <<"Zcash">>};
to_swag(crypto_wallet_currency, {usdt, #{}})             -> #{<<"currency">> => <<"USDT">>};
to_swag(crypto_wallet_currency, {ripple, #{tag := Tag}}) -> #{<<"currency">> => <<"Ripple">>, <<"tag">> => Tag};
to_swag(crypto_wallet_currency, {ripple, #{}})           -> #{<<"currency">> => <<"Ripple">>};

to_swag(withdrawal, State) ->
    Withdrawal = ff_withdrawal_machine:withdrawal(State),
    to_swag(map, maps:merge(
        #{
            <<"id">>          => ff_withdrawal:id(Withdrawal),
            <<"createdAt">>   => to_swag(timestamp, ff_machine:created(State)),
            <<"wallet">>      => ff_withdrawal:wallet_id(Withdrawal),
            <<"destination">> => ff_withdrawal:destination_id(Withdrawal),
            <<"body">>        => to_swag(body, ff_withdrawal:body(Withdrawal)),
            <<"metadata">>    => ff_withdrawal:metadata(Withdrawal),
            ?EXTERNAL_ID      => ff_withdrawal:external_id(Withdrawal)
        },
        to_swag(withdrawal_status, ff_withdrawal:status(Withdrawal))
    ));
to_swag(body, {Amount, Currency}) ->
    to_swag(map, #{
        <<"amount">>   => Amount,
        <<"currency">> => to_swag(currency, Currency)
    });
to_swag(withdrawal_status, pending) ->
    #{<<"status">> => <<"Pending">>};
to_swag(withdrawal_status, succeeded) ->
    #{<<"status">> => <<"Succeeded">>};
to_swag(withdrawal_status, {failed, Failure}) ->
    map_failure(Failure);
to_swag(stat_status_failure, Failure) ->
    map_fistful_stat_error(Failure);
to_swag(withdrawal_event, {EventId, Ts, {status_changed, Status}}) ->
    to_swag(map, #{
        <<"eventID">> => EventId,
        <<"occuredAt">> => to_swag(timestamp, Ts),
        <<"changes">> => [maps:merge(
            #{<<"type">>    => <<"WithdrawalStatusChanged">>},
            to_swag(withdrawal_status, Status)
        )]
    });

to_swag(timestamp, {DateTime, USec}) ->
    DateTimeSeconds = genlib_time:daytime_to_unixtime(DateTime),
    Micros = erlang:convert_time_unit(DateTimeSeconds, second, microsecond),
    genlib_rfc3339:format_relaxed(Micros + USec, microsecond);
to_swag(timestamp_ms, Timestamp) ->
    ff_time:to_rfc3339(Timestamp);
to_swag(currency, Currency) ->
    genlib_string:to_upper(genlib:to_binary(Currency));
to_swag(currency_object, V) ->
    to_swag(map, #{
        <<"id">>          => to_swag(currency, maps:get(id, V)),
        <<"name">>        => maps:get(name, V),
        <<"numericCode">> => genlib:to_binary(maps:get(numcode, V)),
        <<"exponent">>    => maps:get(exponent, V),
        <<"sign">>        => maps:get(sign, V, undefined)
    });
to_swag(is_blocked, {ok, accessible}) ->
    false;
to_swag(is_blocked, _) ->
    true;
to_swag(blocking, unblocked) ->
    false;
to_swag(blocking, blocked) ->
    true;
to_swag(report_object, #ff_reports_Report{
    report_id = ReportID,
    time_range = TimeRange,
    created_at = CreatedAt,
    report_type = Type,
    status = Status,
    file_data_ids = Files
}) ->
    to_swag(map, #{
        <<"id">>        => ReportID,
        <<"fromTime">>  => to_swag(timestamp, TimeRange#ff_reports_ReportTimeRange.from_time),
        <<"toTime">>    => to_swag(timestamp, TimeRange#ff_reports_ReportTimeRange.to_time),
        <<"createdAt">> => to_swag(timestamp, CreatedAt),
        <<"status">>    => to_swag(report_status, Status),
        <<"type">>      => Type,
        <<"files">>     => to_swag(report_files, {files, Files})
    });
to_swag(report_status, pending) ->
    <<"pending">>;
to_swag(report_status, created) ->
    <<"created">>;
to_swag(report_status, canceled) ->
    <<"canceled">>;
to_swag(report_files, {files, undefined}) ->
    [];
to_swag(report_files, {files, Files}) ->
    to_swag({list, report_file}, Files);
to_swag(report_file, File) ->
    #{<<"id">> => File};

to_swag(quote, {#{
    cash_from   := CashFrom,
    cash_to     := CashTo,
    created_at  := CreatedAt,
    expires_on  := ExpiresOn
}, Token}) ->
    #{
        <<"cashFrom">>      => to_swag(body, CashFrom),
        <<"cashTo">>        => to_swag(body, CashTo),
        <<"createdAt">>     => to_swag(timestamp, CreatedAt),
        <<"expiresOn">>     => to_swag(timestamp, ExpiresOn),
        <<"quoteToken">>    => Token
    };

to_swag(p2p_transfer_quote, {Cash, Token, ExpiresOn}) ->
    #{
        <<"customerFee">> => to_swag(body, Cash),
        <<"expiresOn">>   => to_swag(timestamp_ms, ExpiresOn),
        <<"token">>       => Token
    };

to_swag(p2p_transfer, P2PTransferState) ->
    #{
        version := 3,
        id := Id,
        owner := IdentityID,
        body := Cash,
        created_at := CreatedAt,
        sender := {raw, #{contact_info := ContactInfo}},
        sender_resource := Sender,
        receiver_resource := Receiver,
        status := Status
    } = P2PTransfer = p2p_transfer_machine:p2p_transfer(P2PTransferState),
    Metadata = maps:get(<<"metadata">>, get_ctx(P2PTransferState), undefined),
    to_swag(map, #{
        <<"id">> => Id,
        <<"identityID">> => IdentityID,
        <<"createdAt">> => to_swag(timestamp_ms, CreatedAt),
        <<"body">> => to_swag(body, Cash),
        <<"contactInfo">> => to_swag(contact_info, ContactInfo),
        <<"sender">> => to_swag(sender_resource, Sender),
        <<"receiver">> => to_swag(receiver_resource, Receiver),
        <<"status">> => to_swag(p2p_transfer_status, Status),
        <<"externalID">> => maps:get(external_id, P2PTransfer, undefined),
        <<"metadata">> => Metadata
    });

to_swag(p2p_transfer_status, pending) ->
    #{
        <<"status">> => <<"Pending">>
    };
to_swag(p2p_transfer_status, succeeded) ->
    #{
        <<"status">> => <<"Succeeded">>
    };
to_swag(p2p_transfer_status, {failed, P2PTransferFailure}) ->
    map_failure(P2PTransferFailure);

to_swag(contact_info, ContactInfo) ->
    genlib_map:compact(#{
        <<"phoneNumber">> => maps:get(phone_number, ContactInfo, undefined),
        <<"email">> => maps:get(email, ContactInfo, undefined)
    });

to_swag(p2p_template, P2PTemplateState) ->
    #{
        id := ID,
        identity_id := IdentityID,
        details := Details,
        created_at := CreatedAt
    } = P2PTemplate = p2p_template_machine:p2p_template(P2PTemplateState),
    Blocking = p2p_template:blocking(P2PTemplate),
    to_swag(map, #{
        <<"id">> => ID,
        <<"identityID">> => IdentityID,
        <<"isBlocked">> => maybe_to_swag(blocking, Blocking),
        <<"details">> => to_swag(p2p_template_details, Details),
        <<"createdAt">> => to_swag(timestamp_ms, CreatedAt),
        <<"externalID">> => maps:get(external_id, P2PTemplate, undefined)
    });

to_swag(p2p_template_details, Details) ->
    to_swag(map, #{
        <<"body">> => to_swag(p2p_template_body, maps:get(body, Details)),
        <<"metadata">> => maybe_to_swag(p2p_template_metadata, maps:get(metadata, Details, undefined))
    });

to_swag(p2p_template_body, #{value := Body}) ->
    #{<<"value">> => to_swag(map, #{
        <<"currency">> => to_swag(currency, maps:get(currency, Body)),
        <<"amount">> => maybe_to_swag(amount, maps:get(amount, Body, undefined))
    })};

to_swag(p2p_template_metadata, #{value := Metadata}) ->
    #{<<"defaultMetadata">> => Metadata};

to_swag(w2w_transfer, W2WTransferState) ->
    #{
        version := 1,
        id := Id,
        body := Cash,
        created_at := CreatedAt,
        wallet_from_id := Sender,
        wallet_to_id := Receiver,
        status := Status
    } = W2WTransfer = w2w_transfer_machine:w2w_transfer(W2WTransferState),
    to_swag(map, #{
        <<"id">> => Id,
        <<"createdAt">> => to_swag(timestamp_ms, CreatedAt),
        <<"body">> => to_swag(body, Cash),
        <<"sender">> => Sender,
        <<"receiver">> => Receiver,
        <<"status">> => to_swag(w2w_transfer_status, Status),
        <<"externalID">> => maps:get(external_id, W2WTransfer, undefined)
    });

to_swag(w2w_transfer_status, pending) ->
    #{
        <<"status">> => <<"Pending">>
    };
to_swag(w2w_transfer_status, succeeded) ->
    #{
        <<"status">> => <<"Succeeded">>
    };
to_swag(w2w_transfer_status, {failed, W2WTransferFailure}) ->
    map_failure(W2WTransferFailure);
to_swag(sub_failure, #{
    code := Code
} = SubError) ->
    to_swag(map, #{
        <<"code">> => Code,
        <<"subError">> => to_swag(sub_failure, maps:get(failure, SubError, undefined))
    });
to_swag(sub_failure, undefined) ->
    undefined;

to_swag(webhook, #webhooker_Webhook{
    id = ID,
    identity_id = IdentityID,
    wallet_id = WalletID,
    event_filter = EventFilter,
    url = URL,
    pub_key = PubKey,
    enabled = Enabled
}) ->
    to_swag(map, #{
        <<"id">> => integer_to_binary(ID),
        <<"identityID">> => IdentityID,
        <<"walletID">> => WalletID,
        <<"active">> => to_swag(boolean, Enabled),
        <<"scope">> => to_swag(webhook_scope, EventFilter),
        <<"url">> => URL,
        <<"publicKey">> => PubKey
    });

to_swag(webhook_scope, #webhooker_EventFilter{types = EventTypes}) ->
    List = to_swag({set, webhook_event_types}, EventTypes),
    lists:foldl(fun({Topic, Type}, Acc) ->
        case maps:get(<<"topic">>, Acc, undefined) of
            undefined ->
                Acc#{
                    <<"topic">> => to_swag(webhook_topic, Topic),
                    <<"eventTypes">> => [Type]
                };
            _ ->
                #{<<"eventTypes">> := Types} = Acc,
                Acc#{
                    <<"eventTypes">> := [Type | Types]
                }
        end
    end, #{}, List);

to_swag(webhook_event_types, {withdrawal, EventType}) ->
    {withdrawal, to_swag(webhook_withdrawal_event_types, EventType)};
to_swag(webhook_event_types, {destination, EventType}) ->
    {destination, to_swag(webhook_destination_event_types, EventType)};

to_swag(webhook_topic, withdrawal) ->
    <<"WithdrawalsTopic">>;
to_swag(webhook_topic, destination) ->
    <<"DestinationsTopic">>;

to_swag(webhook_withdrawal_event_types, {started, _}) ->
    <<"WithdrawalStarted">>;
to_swag(webhook_withdrawal_event_types, {succeeded, _}) ->
    <<"WithdrawalSucceeded">>;
to_swag(webhook_withdrawal_event_types, {failed, _}) ->
    <<"WithdrawalFailed">>;

to_swag(webhook_destination_event_types, {created, _}) ->
    <<"DestinationCreated">>;
to_swag(webhook_destination_event_types, {unauthorized, _}) ->
    <<"DestinationUnauthorized">>;
to_swag(webhook_destination_event_types, {authorized, _}) ->
    <<"DestinationAuthorized">>;

to_swag(boolean, true) ->
    true;
to_swag(boolean, false) ->
    false;
to_swag({list, Type}, List) ->
    lists:map(fun(V) -> to_swag(Type, V) end, List);
to_swag({set, Type}, Set) ->
    to_swag({list, Type}, ordsets:to_list(Set));
to_swag(map, Map) ->
    genlib_map:compact(Map);
to_swag(_, V) ->
    V.

maybe_to_swag(_T, undefined) ->
    undefined;
maybe_to_swag(T, V) ->
    to_swag(T, V).

map_failure(#{code := Code} = Err) ->
    #{
        <<"status">> => <<"Failed">>,
        <<"failure">> => to_swag(map, #{
            <<"code">> => Code,
            <<"subError">> => map_subfailure(maps:get(sub, Err, undefined))
        })
    }.

map_fistful_stat_error(_Reason) ->
    #{
        <<"code">> => <<"failed">>
    }.

map_subfailure(undefined) ->
    undefined;

map_subfailure(#{code := Code} = Subfailure) ->
    to_swag(map, #{
        <<"code">> => Code,
        <<"subError">> => map_subfailure(maps:get(sub, Subfailure, undefined))
    }).

authorize_withdrawal(Params, Context) ->
    _ = authorize_resource(wallet, Params, Context),
    _ = authorize_resource(destination, Params, Context).

authorize_resource(Resource, Params, Context) ->
    %% TODO
    %%  - ff_pipeline:do/1 would make the code rather more clear here.
    case authorize_resource_by_grant(Resource, Params) of
        ok ->
            ok;
        {error, missing} ->
            authorize_resource_by_bearer(Resource, Params, Context)
    end.

authorize_resource_by_bearer(Resource, Params, Context) ->
    _ = get_state(Resource, maps:get(genlib:to_binary(Resource), Params), Context),
    ok.

authorize_resource_by_grant(R = destination, #{
    <<"destination">>      := ID,
    <<"destinationGrant">> := Grant
}) ->
    authorize_resource_by_grant(R, Grant, get_resource_accesses(R, ID, write), undefined);
authorize_resource_by_grant(R = wallet, #{
    <<"wallet">>      := ID,
    <<"walletGrant">> := Grant,
    <<"body">>        := WithdrawalBody
}) ->
    authorize_resource_by_grant(R, Grant, get_resource_accesses(R, ID, write), WithdrawalBody);
authorize_resource_by_grant(_, _) ->
    {error, missing}.

authorize_resource_by_grant(Resource, Grant, Access, Params) ->
    {_, _, Claims} = unwrap(Resource, uac_authorizer_jwt:verify(Grant, #{})),
    _ = unwrap(Resource, verify_access(Access, Claims)),
    _ = unwrap(Resource, verify_claims(Resource, Claims, Params)).

get_resource_accesses(Resource, ID, Permission) ->
    [{get_resource_accesses(Resource, ID), Permission}].

get_resource_accesses(destination, ID) ->
    [party, {destinations, ID}];
get_resource_accesses(wallet, ID) ->
    [party, {wallets, ID}].

verify_access(Access, #{<<"resource_access">> := #{?DOMAIN := ACL}}) ->
    do_verify_access(Access, ACL);
verify_access(Access, #{<<"resource_access">> := #{<<"common-api">> := ACL}}) -> % Legacy grants support
    do_verify_access(Access, ACL);
verify_access(_, _) ->
    {error, {unauthorized, {grant, insufficient_access}}}.

do_verify_access(Access, ACL) ->
    case lists:all(
        fun ({Scope, Permission}) -> lists:member(Permission, uac_acl:match(Scope, ACL)) end,
        Access
    ) of
        true  -> ok;
        false -> {error, {unauthorized, {grant, insufficient_access}}}
    end.

verify_claims(destination, _Claims, _) ->
    ok;
verify_claims(wallet,
    #{<<"amount">> := GrantAmount, <<"currency">> := Currency},
    #{<<"amount">> := ReqAmount,   <<"currency">> := Currency}
) when GrantAmount >= ReqAmount ->
    ok;
verify_claims(_, _, _) ->
    {error, {unauthorized, {grant, insufficient_claims}}}.

issue_quote_token(PartyID, Data) ->
    uac_authorizer_jwt:issue(wapi_utils:get_unique_id(), PartyID, Data, wapi_auth:get_signee()).

-spec get_p2p_quote_surplus(p2p_quote:quote()) ->
    ff_cash:cash().
get_p2p_quote_surplus(Quote) ->
    Fees = p2p_quote:fees(Quote),
    case ff_fees_final:surplus(Fees) of
        undefined ->
            erlang:error({no_surplus, Fees}, [Quote]);
        Cash ->
            Cash
    end.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec date_time_convertion_test() -> _.
date_time_convertion_test() ->
    ?assertEqual(<<"2020-05-25T12:34:56.123456Z">>, to_swag(timestamp, {{{2020, 05, 25}, {12, 34, 56}}, 123456})).

-endif.
