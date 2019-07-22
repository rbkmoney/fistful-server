-module(wapi_wallet_ff_backend).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_fistful_stat_thrift.hrl").
-include_lib("file_storage_proto/include/fs_file_storage_thrift.hrl").
-include_lib("fistful_reporter_proto/include/ff_reporter_reports_thrift.hrl").
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
-export([create_wallet/2]).
-export([get_wallet_account/2]).
-export([list_wallets/2]).

-export([get_destinations/2]).
-export([get_destination/2]).
-export([create_destination/2]).
-export([create_withdrawal/2]).
-export([get_withdrawal/2]).
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

%% Types

-type ctx()         :: wapi_handler:context().
-type params()      :: map().
-type id()          :: binary() | undefined.
-type result()      :: result(map()).
-type result(T)     :: result(T, notfound).
-type result(T, E)  :: {ok, T} | {error, E}.
-type result_stat() :: {200 | 400, list(), map()}.

-define(CTX_NS, <<"com.rbkmoney.wapi">>).
-define(PARAMS_HASH, <<"params_hash">>).
-define(EXTERNAL_ID, <<"externalID">>).

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
    {email, notfound}          |
    {conflict, id()}
).
create_identity(Params, Context) ->
    CreateIdentity = fun(ID, EntityCtx) ->
        ff_identity_machine:create(
            ID,
            maps:merge(from_swag(identity_params, Params), #{party => wapi_handler_utils:get_owner(Context)}),
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
    ChallengeId = make_id(identity_challenge),
    do(fun() ->
        _ = check_resource(identity, IdentityId, Context),
        ok = unwrap(ff_identity_machine:start_challenge(IdentityId,
            maps:merge(#{id => ChallengeId}, from_swag(identity_challenge_params, Params)
        ))),
        unwrap(get_identity_challenge(IdentityId, ChallengeId, Context))
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
    get_events({identity, challenge_event}, IdentityId, Limit, Cursor, Filter, Context).

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
    get_event({identity, challenge_event}, IdentityId, EventId, Mapper, Context).

%% Wallets

-spec get_wallet(id(), ctx()) -> result(map(),
    {wallet, notfound}     |
    {wallet, unauthorized}
).
get_wallet(WalletID, Context) ->
    do(fun() -> to_swag(wallet, get_state(wallet, WalletID, Context)) end).

-spec create_wallet(params(), ctx()) -> result(map(),
    invalid                  |
    {identity, unauthorized} |
    {identity, notfound}     |
    {currency, notfound}     |
    {conflict, id()}         |
    {inaccessible, _}
).
create_wallet(Params = #{<<"identity">> := IdenityId}, Context) ->
    CreateFun = fun(ID, EntityCtx) ->
        _ = check_resource(identity, IdenityId, Context),
        ff_wallet_machine:create(
            ID,
            from_swag(wallet_params, Params),
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
            ff_account:accounter_account_id(Account)
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

-spec create_destination(params(), ctx()) -> result(map(),
    invalid                     |
    {identity, unauthorized}    |
    {identity, notfound}        |
    {currency, notfound}        |
    {inaccessible, _}           |
    {conflict, id()}
).
create_destination(Params = #{<<"identity">> := IdenityId}, Context) ->
    CreateFun = fun(ID, EntityCtx) ->
        _ = check_resource(identity, IdenityId, Context),
        ff_destination:create(
            ID,
            from_swag(destination_params, Params),
            add_meta_to_ctx([], Params, EntityCtx)
        )
    end,
    do(fun() -> unwrap(create_entity(destination, Params, CreateFun, Context)) end).

-spec create_withdrawal(params(), ctx()) -> result(map(),
    {source, notfound}            |
    {destination, notfound}       |
    {destination, unauthorized}   |
    {conflict, id()}              |
    {provider, notfound}          |
    {wallet, {inaccessible, _}}   |
    {wallet, {currency, invalid}} |
    {wallet, {provider, invalid}} |
    {quote_invalid_party, _}      |
    {quote_invalid_wallet, _}     |
    {quote, {invalid_body, _}}    |
    {quote, {invalid_destination, _}}
).
create_withdrawal(Params, Context) ->
    CreateFun = fun(ID, EntityCtx) ->
        Quote = unwrap(maybe_check_quote_token(Params, Context)),
        WithdrawalParams = from_swag(withdrawal_params, Params),
        ff_withdrawal:create(
            ID,
            genlib_map:compact(WithdrawalParams#{quote => Quote}),
            add_meta_to_ctx([], Params, EntityCtx)
        )
    end,
    do(fun() -> unwrap(create_entity(withdrawal, Params, CreateFun, Context)) end).

-spec get_withdrawal(id(), ctx()) -> result(map(),
    {withdrawal, unauthorized} |
    {withdrawal, notfound}
).
get_withdrawal(WithdrawalId, Context) ->
    do(fun() -> to_swag(withdrawal, get_state(withdrawal, WithdrawalId, Context)) end).

-spec get_withdrawal_events(params(), ctx()) -> result([map()],
    {withdrawal, unauthorized} |
    {withdrawal, notfound}
).
get_withdrawal_events(Params = #{'withdrawalID' := WithdrawalId, 'limit' := Limit}, Context) ->
    Cursor = genlib_map:get('eventCursor', Params),
    Filter = fun
        ({ID, {ev, Ts, Body = {status_changed, _}}}) ->
            {true, {ID, Ts, Body}};
        (_) ->
            false
    end,
    get_events({withdrawal, event}, WithdrawalId, Limit, Cursor, Filter, Context).

-spec get_withdrawal_event(id(), integer(), ctx()) -> result(map(),
    {withdrawal, unauthorized} |
    {withdrawal, notfound}     |
    {event, notfound}
).
get_withdrawal_event(WithdrawalId, EventId, Context) ->
    Mapper = fun
        ({ID, {ev, Ts, Body = {status_changed, _}}}) when ID =:= EventId ->
            {true, {ID, Ts, Body}};
        (_) ->
            false
    end,
    get_event({withdrawal, event}, WithdrawalId, EventId, Mapper, Context).

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

%% Internal functions

maybe_check_quote_token(Params = #{<<"quoteToken">> := QuoteToken}, Context) ->
    {ok, JSONData} = wapi_signer:verify(QuoteToken),
    Data = jsx:decode(JSONData, [return_maps]),
    unwrap(quote_invalid_party,
        valid(
            maps:get(<<"partyID">>, Data),
            wapi_handler_utils:get_owner(Context)
    )),
    unwrap(quote_invalid_wallet,
        valid(
            maps:get(<<"walletID">>, Data),
            maps:get(<<"wallet">>, Params)
    )),
    check_quote_destination(
        maps:get(<<"destinationID">>, Data, undefined),
        maps:get(<<"destination">>, Params)
    ),
    check_quote_body(maps:get(<<"cashFrom">>, Data), maps:get(<<"body">>, Params)),
    {ok, #{
        cash_from   => from_swag(withdrawal_body, maps:get(<<"cashFrom">>, Data)),
        cash_to     => from_swag(withdrawal_body, maps:get(<<"cashTo">>, Data)),
        created_at  => maps:get(<<"createdAt">>, Data),
        expires_on  => maps:get(<<"expiresOn">>, Data),
        quote_data  => maps:get(<<"quoteData">>, Data)
    }};
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

create_quote_token(#{
    cash_from   := CashFrom,
    cash_to     := CashTo,
    created_at  := CreatedAt,
    expires_on  := ExpiresOn,
    quote_data  := QuoteData
}, WalletID, DestinationID, PartyID) ->
    Data = genlib_map:compact(#{
        <<"version">>       => 1,
        <<"walletID">>      => WalletID,
        <<"destinationID">> => DestinationID,
        <<"partyID">>       => PartyID,
        <<"cashFrom">>      => to_swag(withdrawal_body, CashFrom),
        <<"cashTo">>        => to_swag(withdrawal_body, CashTo),
        <<"createdAt">>     => to_swag(timestamp, CreatedAt),
        <<"expiresOn">>     => to_swag(timestamp, ExpiresOn),
        <<"quoteData">>     => QuoteData
    }),
    JSONData = jsx:encode(Data),
    {ok, Token} = wapi_signer:sign(JSONData),
    Token.

filter_identity_challenge_status(Filter, Status) ->
    maps:get(<<"status">>, to_swag(challenge_status, Status)) =:= Filter.

get_event(Type, ResourceId, EventId, Mapper, Context) ->
    case get_events(Type, ResourceId, 1, EventId - 1, Mapper, Context) of
        {ok, [Event]}      -> {ok, Event};
        {ok, []}           -> {error, {event, notfound}};
        Error = {error, _} -> Error
    end.

get_events(Type = {Resource, _}, ResourceId, Limit, Cursor, Filter, Context) ->
    do(fun() ->
        _ = check_resource(Resource, ResourceId, Context),
        to_swag(
            {list, get_event_type(Type)},
            collect_events(get_collector(Type, ResourceId), Filter, Cursor, Limit)
        )
    end).

get_event_type({identity, challenge_event}) -> identity_challenge_event;
get_event_type({withdrawal, event})         -> withdrawal_event.

get_collector({identity, challenge_event}, Id) ->
    fun(C, L) -> unwrap(ff_identity_machine:events(Id, {C, L, forward})) end;
get_collector({withdrawal, event}, Id) ->
    fun(C, L) -> unwrap(ff_withdrawal:events(Id, {C, L, forward})) end.

collect_events(Collector, Filter, Cursor, Limit) ->
    collect_events(Collector, Filter, Cursor, Limit, []).

collect_events(Collector, Filter, Cursor, Limit, Acc) when Limit =:= undefined ->
    case Collector(Cursor, Limit) of
        Events1 when length(Events1) > 0 ->
            {_, Events2} = filter_events(Filter, Events1),
            Acc ++ Events2;
        [] ->
            Acc
    end;
collect_events(Collector, Filter, Cursor, Limit, Acc) ->
    case Collector(Cursor, Limit) of
        Events1 when length(Events1) > 0 ->
            {CursorNext, Events2} = filter_events(Filter, Events1),
            collect_events(Collector, Filter, CursorNext, Limit - length(Events2), Acc ++ Events2);
        [] ->
            Acc
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

do_get_state(identity,    Id) -> ff_identity_machine:get(Id);
do_get_state(wallet,      Id) -> ff_wallet_machine:get(Id);
do_get_state(destination, Id) -> ff_destination:get_machine(Id);
do_get_state(withdrawal,  Id) -> ff_withdrawal:get_machine(Id).

check_resource(Resource, Id, Context) ->
    _ = get_state(Resource, Id, Context),
    ok.

make_ctx(Context) ->
    #{?CTX_NS => #{<<"owner">> => wapi_handler_utils:get_owner(Context)}}.

add_meta_to_ctx(WapiKeys, Params, Context = #{?CTX_NS := Ctx}) ->
    Context#{?CTX_NS => maps:merge(
        Ctx,
        maps:with([<<"metadata">> | WapiKeys], Params)
    )}.

add_to_ctx(Key, Value, Context = #{?CTX_NS := Ctx}) ->
    Context#{?CTX_NS => Ctx#{Key => Value}}.

get_ctx(State) ->
    unwrap(ff_ctx:get(?CTX_NS, ff_machine:ctx(State))).

get_hash(State) ->
    maps:get(?PARAMS_HASH, get_ctx(State)).

get_resource_owner(State) ->
    maps:get(<<"owner">>, get_ctx(State)).

is_resource_owner(HandlerCtx, State) ->
    wapi_handler_utils:get_owner(HandlerCtx) =:= get_resource_owner(State).

check_resource_access(HandlerCtx, State) ->
    check_resource_access(is_resource_owner(HandlerCtx, State)).

check_resource_access(true)  -> ok;
check_resource_access(false) -> {error, unauthorized}.

create_entity(Type, Params, CreateFun, Context) ->
    ID = make_id(Type, construct_external_id(Params, Context)),
    Hash = erlang:phash2(Params),
    case CreateFun(ID, add_to_ctx(?PARAMS_HASH, Hash, make_ctx(Context))) of
        ok ->
            do(fun() -> to_swag(Type, get_state(Type, ID, Context)) end);
        {error, exists} ->
            get_and_compare_hash(Type, ID, Hash, Context);
        {error, E} ->
            throw(E)
    end.

get_and_compare_hash(Type, ID, Hash, Context) ->
    case do(fun() -> get_state(Type, ID, Context) end) of
        {ok, State} ->
            compare_hash(Hash, get_hash(State), {ID, to_swag(Type, State)});
        Error ->
            Error
    end.

compare_hash(Hash, Hash, {_, Data}) ->
    {ok, Data};
compare_hash(_, _, {ID, _}) ->
    {error, {conflict, ID}}.

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
    case wapi_auth:get_claim(<<"email">>, AuthContext, undefined) of
        undefined -> {error, {email, notfound}};
        Email     -> {ok, Email}
    end.

-spec not_implemented() -> no_return().
not_implemented() ->
    wapi_handler_utils:throw_not_implemented().

do(Fun) ->
    ff_pipeline:do(Fun).

unwrap(Res) ->
    ff_pipeline:unwrap(Res).

unwrap(Tag, Res) ->
    ff_pipeline:unwrap(Tag, Res).

valid(Val1, Val2) ->
    ff_pipeline:valid(Val1, Val2).

get_contract_id_from_identity(IdentityID, Context) ->
    State = get_state(identity, IdentityID, Context),
    ff_identity:contract(ff_machine:model(State)).

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

%% ID Gen

make_id(Type) ->
    make_id(Type, undefined).

make_id(Type, ExternalID) ->
    unwrap(ff_external_id:check_in(Type, ExternalID)).

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
        <<"failure">> => #{
            <<"code">> => to_swag(stat_status_failure, Failure)
        }
    }.

decode_deposit_stat_status({pending, #fistfulstat_DepositPending{}}) ->
    #{<<"status">> => <<"Pending">>};
decode_deposit_stat_status({succeeded, #fistfulstat_DepositSucceeded{}}) ->
    #{<<"status">> => <<"Succeeded">>};
decode_deposit_stat_status({failed, #fistfulstat_DepositFailed{failure = Failure}}) ->
    #{
        <<"status">> => <<"Failed">>,
        <<"failure">> => #{
            <<"code">> => to_swag(stat_status_failure, Failure)
        }
    }.

construct_external_id(Params, Context) ->
    case genlib_map:get(?EXTERNAL_ID, Params) of
        undefined ->
            undefined;
        ExternalID ->
            PartyID = wapi_handler_utils:get_owner(Context),
            <<PartyID/binary, "/", ExternalID/binary>>
    end.

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
        body            => from_swag(withdrawal_body, maps:get(<<"cash">>, Params)),
        destination_id  => maps:get(<<"destinationID">>, Params, undefined)
    }, Params));
from_swag(identity_params, Params) ->
    add_external_id(#{
        provider => maps:get(<<"provider">>, Params),
        class    => maps:get(<<"class">>   , Params)
    }, Params);
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
    add_external_id(#{
        identity => maps:get(<<"identity">>, Params),
        currency => maps:get(<<"currency">>, Params),
        name     => maps:get(<<"name">>    , Params)
    }, Params);
from_swag(destination_params, Params) ->
    add_external_id(#{
        identity => maps:get(<<"identity">>, Params),
        currency => maps:get(<<"currency">>, Params),
        name     => maps:get(<<"name">>    , Params),
        resource => from_swag(destination_resource, maps:get(<<"resource">>, Params))
    }, Params);
from_swag(destination_resource, #{
    <<"type">> := <<"BankCardDestinationResource">>,
    <<"token">> := WapiToken
}) ->
    BankCard = wapi_utils:base64url_to_map(WapiToken),
    {bank_card, #{
        token          => maps:get(<<"token">>, BankCard),
        payment_system => erlang:binary_to_existing_atom(maps:get(<<"paymentSystem">>, BankCard), latin1),
        bin            => maps:get(<<"bin">>, BankCard),
        masked_pan     => maps:get(<<"lastDigits">>, BankCard)
    }};
from_swag(destination_resource, Resource = #{
    <<"type">>     := <<"CryptoWalletDestinationResource">>,
    <<"id">>       := CryptoWalletID,
    <<"currency">> := CryptoWalletCurrency
}) ->
    Tag = maps:get(<<"tag">>, Resource, undefined),
    {crypto_wallet, genlib_map:compact(#{
        id       => CryptoWalletID,
        currency => from_swag(crypto_wallet_currency, CryptoWalletCurrency),
        tag      => Tag
    })};

from_swag(crypto_wallet_currency, <<"Bitcoin">>)     -> bitcoin;
from_swag(crypto_wallet_currency, <<"Litecoin">>)    -> litecoin;
from_swag(crypto_wallet_currency, <<"BitcoinCash">>) -> bitcoin_cash;
from_swag(crypto_wallet_currency, <<"Ripple">>)      -> ripple;
from_swag(crypto_wallet_currency, <<"Ethereum">>)    -> ethereum;
from_swag(crypto_wallet_currency, <<"Zcash">>)       -> zcash;

from_swag(withdrawal_params, Params) ->
    add_external_id(#{
        wallet_id      => maps:get(<<"wallet">>     , Params),
        destination_id => maps:get(<<"destination">>, Params),
        body           => from_swag(withdrawal_body , maps:get(<<"body">>, Params))
    }, Params);
%% TODO
%%  - remove this clause when we fix negative accounts and turn on validation in swag
from_swag(withdrawal_body, #{<<"amount">> := Amount}) when Amount < 0 ->
    wapi_handler:throw_result(wapi_handler_utils:reply_error(400, #{<<"errorType">> => <<"WrongSize">>}));
from_swag(withdrawal_body, Body) ->
    {genlib:to_int(maps:get(<<"amount">>, Body)), maps:get(<<"currency">>, Body)};
from_swag(currency, V) ->
    V;
from_swag(residence, V) ->
    try erlang:binary_to_existing_atom(genlib_string:to_lower(V), latin1) catch
        error:badarg ->
            % TODO
            %  - Essentially this is incorrect, we should reply with 400 instead
            undefined
    end;

from_swag({list, Type}, List) ->
    lists:map(fun(V) -> from_swag(Type, V) end, List).

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
        <<"metadata">>           => maps:get(<<"metadata">>, WapiCtx, undefined),
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
to_swag(challenge_failure_reason, Failure = #domain_Failure{}) ->
    to_swag(domain_failure, Failure);
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

to_swag(wallet, State) ->
    Wallet = ff_wallet_machine:wallet(State),
    WapiCtx = get_ctx(State),
    to_swag(map, #{
        <<"id">>         => ff_wallet:id(Wallet),
        <<"name">>       => ff_wallet:name(Wallet),
        <<"createdAt">>  => to_swag(timestamp, ff_machine:created(State)),
        <<"isBlocked">>  => to_swag(is_blocked, ff_wallet:is_accessible(Wallet)),
        <<"identity">>   => ff_wallet:identity(Wallet),
        <<"currency">>   => to_swag(currency, ff_wallet:currency(Wallet)),
        <<"metadata">>   => genlib_map:get(<<"metadata">>, WapiCtx),
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
    WapiCtx = get_ctx(State),
    to_swag(map, maps:merge(
        #{
            <<"id">>         => ff_destination:id(Destination),
            <<"name">>       => ff_destination:name(Destination),
            <<"createdAt">>  => to_swag(timestamp, ff_machine:created(State)),
            <<"isBlocked">>  => to_swag(is_blocked, ff_destination:is_accessible(Destination)),
            <<"identity">>   => ff_destination:identity(Destination),
            <<"currency">>   => to_swag(currency, ff_destination:currency(Destination)),
            <<"resource">>   => to_swag(destination_resource, ff_destination:resource(Destination)),
            <<"metadata">>   => genlib_map:get(<<"metadata">>, WapiCtx),
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
to_swag(destination_resource, {bank_card, BankCard}) ->
    to_swag(map, #{
        <<"type">>          => <<"BankCardDestinationResource">>,
        <<"token">>         => maps:get(token, BankCard),
        <<"paymentSystem">> => genlib:to_binary(genlib_map:get(payment_system, BankCard)),
        <<"bin">>           => genlib_map:get(bin, BankCard),
        <<"lastDigits">>    => to_swag(pan_last_digits, genlib_map:get(masked_pan, BankCard))
    });
to_swag(destination_resource, {crypto_wallet, CryptoWallet}) ->
    to_swag(map, #{
        <<"type">>     => <<"CryptoWalletDestinationResource">>,
        <<"id">>       => maps:get(id, CryptoWallet),
        <<"currency">> => to_swag(crypto_wallet_currency, maps:get(currency, CryptoWallet)),
        <<"tag">>      => maps:get(tag, CryptoWallet, undefined)
    });

to_swag(pan_last_digits, MaskedPan) ->
    wapi_utils:get_last_pan_digits(MaskedPan);

to_swag(crypto_wallet_currency, bitcoin)      -> <<"Bitcoin">>;
to_swag(crypto_wallet_currency, litecoin)     -> <<"Litecoin">>;
to_swag(crypto_wallet_currency, bitcoin_cash) -> <<"BitcoinCash">>;
to_swag(crypto_wallet_currency, ripple)       -> <<"Ripple">>;
to_swag(crypto_wallet_currency, ethereum)     -> <<"Ethereum">>;
to_swag(crypto_wallet_currency, zcash)        -> <<"Zcash">>;

to_swag(withdrawal, State) ->
    Withdrawal = ff_withdrawal:get(State),
    WapiCtx = get_ctx(State),
    to_swag(map, maps:merge(
        #{
            <<"id">>          => ff_withdrawal:id(Withdrawal),
            <<"createdAt">>   => to_swag(timestamp, ff_machine:created(State)),
            <<"wallet">>      => ff_withdrawal:wallet_id(Withdrawal),
            <<"destination">> => ff_withdrawal:destination_id(Withdrawal),
            <<"body">>        => to_swag(withdrawal_body, ff_withdrawal:body(Withdrawal)),
            <<"metadata">>    => genlib_map:get(<<"metadata">>, WapiCtx),
            ?EXTERNAL_ID      => ff_withdrawal:external_id(Withdrawal)
        },
        to_swag(withdrawal_status, ff_withdrawal:status(Withdrawal))
    ));
to_swag(withdrawal_body, {Amount, Currency}) ->
    to_swag(map, #{
        <<"amount">>   => Amount,
        <<"currency">> => to_swag(currency, Currency)
    });
to_swag(withdrawal_status, pending) ->
    #{<<"status">> => <<"Pending">>};
to_swag(withdrawal_status, succeeded) ->
    #{<<"status">> => <<"Succeeded">>};
to_swag(withdrawal_status, {failed, Failure}) ->
    #{
        <<"status">> => <<"Failed">>,
        <<"failure">> => #{
            <<"code">> => to_swag(withdrawal_status_failure, Failure)
        }
    };
to_swag(withdrawal_status_failure, Failure = #domain_Failure{}) ->
    to_swag(domain_failure, Failure);
to_swag(withdrawal_status_failure, Failure) ->
    to_swag(domain_failure, map_internal_error(Failure));
to_swag(stat_status_failure, Failure) ->
    to_swag(domain_failure, map_fistful_stat_error(Failure));
to_swag(withdrawal_event, {EventId, Ts, {status_changed, Status}}) ->
    to_swag(map, #{
        <<"eventID">> => EventId,
        <<"occuredAt">> => to_swag(timestamp, Ts),
        <<"changes">> => [maps:merge(
            #{<<"type">>    => <<"WithdrawalStatusChanged">>},
            to_swag(withdrawal_status, Status)
        )]
    });

to_swag(timestamp, {{Date, Time}, Usec}) ->
    {ok, Timestamp} = rfc3339:format({Date, Time, Usec, undefined}), % nowarn this?
    Timestamp;
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
to_swag(domain_failure, Failure = #domain_Failure{}) ->
    erlang:list_to_binary(payproc_errors:format_raw(Failure));
to_swag(is_blocked, {ok, accessible}) ->
    false;
to_swag(is_blocked, _) ->
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
        <<"cashFrom">>      => to_swag(withdrawal_body, CashFrom),
        <<"cashTo">>        => to_swag(withdrawal_body, CashTo),
        <<"createdAt">>     => to_swag(timestamp, CreatedAt),
        <<"expiresOn">>     => to_swag(timestamp, ExpiresOn),
        <<"quoteToken">>    => Token
    };

to_swag({list, Type}, List) ->
    lists:map(fun(V) -> to_swag(Type, V) end, List);
to_swag(map, Map) ->
    genlib_map:compact(Map);
to_swag(_, V) ->
    V.

map_internal_error({wallet_limit, {terms_violation, {cash_range, _Details}}}) ->
    #domain_Failure{
        code = <<"terms_violation">>,
        sub = #domain_SubFailure{
            code = <<"cash_range">>
        }
    };
map_internal_error(_Reason) ->
    #domain_Failure{
        code = <<"failed">>
    }.

map_fistful_stat_error(_Reason) ->
    #domain_Failure{
        code = <<"failed">>
    }.
