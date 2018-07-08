%% Temporary stab for wallet handler

-module(wapi_wallet_ff_backend).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

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
-export([get_identity_challengies/2]).
-export([create_identity_challenge/3]).
-export([get_identity_challenge/3]).
-export([cancel_identity_challenge/3]).
-export([get_identity_challenge_events/2]).
-export([get_identity_challenge_event/2]).

-export([get_wallet/2]).
-export([create_wallet/2]).
-export([get_wallet_account/2]).

-export([get_destinations/2]).
-export([get_destination/2]).
-export([create_destination/2]).
-export([create_withdrawal/2]).
-export([get_withdrawal/2]).
-export([get_withdrawal_events/2]).
-export([get_withdrawal_event/3]).

-export([get_residence/2]).
-export([get_currency/2]).

%% API

-type wctx() :: woody_context:ctx().
-type result(T)    :: result(T, notfound).
-type result(T, E) :: {ok, T} | {error, E}.

%% Providers

-spec get_providers(_, _) -> no_return().
get_providers(_Params, _Context) ->
    not_implemented().

-spec get_provider(_, _) -> _.
get_provider(ProviderId, _Context) ->
    do(fun() -> to_swag(provider, unwrap(ff_provider:get(ProviderId))) end).

-spec get_provider_identity_classes(_, _) -> _.
get_provider_identity_classes(Id, _Context) ->
    do(fun() ->
        Provider = unwrap(ff_provider:get(Id)),
        lists:map(
            fun(ClassId) -> get_provider_identity_class(ClassId, Provider) end,
            ff_provider:list_identity_classes(Provider)
        )
    end).

-spec get_provider_identity_class(_, _, _) -> _.
get_provider_identity_class(ProviderId, ClassId, _Context) ->
    do(fun() -> get_provider_identity_class(ClassId, unwrap(ff_provider:get(ProviderId))) end).

get_provider_identity_class(ClassId, Provider) ->
    to_swag(identity_class, unwrap(ff_provider:get_identity_class(ClassId, Provider))).

-spec get_provider_identity_class_levels(_, _, _) -> no_return().
get_provider_identity_class_levels(_ProviderId, _ClassId, _Context) ->
    not_implemented().

-spec get_provider_identity_class_level(_, _, _, _) -> no_return().
get_provider_identity_class_level(_ProviderId, _ClassId, _LevelId, _Context) ->
    not_implemented().

%% Identities

-spec get_identities(_, _) -> no_return().
get_identities(_Params, _Context) ->
    not_implemented().

-spec get_identity(_, _) -> _.
get_identity(IdentityId, _Context) ->
    do(fun() -> to_swag(identity, unwrap(ff_identity_machine:get(IdentityId))) end).

-spec create_identity(_, _) -> _.
create_identity(Params = #{<<"party">> := PartyId}, Context) ->
    IdentityId = genlib:unique(),
    with_party(PartyId, fun() ->
        case ff_identity_machine:create(IdentityId, from_swag(identity_params, Params), make_ctx(Params, [<<"name">>])) of
            ok ->
                ok = scoper:add_meta(#{identity_id => IdentityId}),
                ok = lager:info("Identity created"),
                get_identity(IdentityId, Context);
            Error = {error, _} ->
                Error
        end
    end).

-spec get_identity_challengies(_, _) -> no_return().
get_identity_challengies(_Params, _Context) ->
    not_implemented().

-spec create_identity_challenge(_, _, _) -> _.
create_identity_challenge(IdentityId, Params, Context) ->
    ChallengeId = genlib:unique(),
    case ff_identity_machine:start_challenge(
        IdentityId,
        maps:merge(#{id => ChallengeId}, from_swag(identity_challenge_params, Params))
    ) of
        ok ->
            get_identity_challenge(IdentityId, ChallengeId, Context);
        {error, notfound} ->
            {error, {identity, notfound}};
        Error = {error, _} ->
            Error
    end.

-spec get_identity_challenge(_, _, _) -> _.
get_identity_challenge(IdentityId, ChallengeId, Context) ->
    case ff_identity_machine:get(IdentityId) of
        {ok, IdentityState} ->
             case ff_identity:challenge(ChallengeId, ff_identity_machine:identity(IdentityState)) of
                 {ok, Challenge} ->
                     Proofs = [
                         wapi_privdoc_handler:get_proof(Token, Context) ||
                         {_, Token} <- ff_identity_challenge:proofs(Challenge)
                     ],
                     {ok, to_swag(identity_challenge, {ChallengeId, Challenge, Proofs})};
                 Error = {error, notfound} ->
                     Error
             end;
        Error = {error, notfound} ->
            Error
    end.

-spec cancel_identity_challenge(_, _, _) -> no_return().
cancel_identity_challenge(_IdentityId, _ChallengeId, _Context) ->
    not_implemented().

-spec get_identity_challenge_events(_, wctx()) ->
    {ok, [map()]} |
    {error, _}.
get_identity_challenge_events(Params = #{
    'identityID'  := IdentityId,
    'challengeID' := ChallengeId,
    'limit'  := Limit
}, _Context) ->
    Cursor = genlib_map:get('eventCursor', Params),
    Filter = fun
        ({ID, {ev, Ts, {challenge, I, Body = {status_changed, _}}}}) when I =:= ChallengeId ->
            {true, {ID, Ts, Body}};
        (_) ->
            false
    end,
    do_get_identity_challenge_events(IdentityId, Limit, Cursor, Filter).

-spec get_identity_challenge_event(_, _) -> _.
get_identity_challenge_event(#{
    'identityID'  := IdentityId,
    'challengeID' := ChallengeId,
    'eventID'     := EventId
}, _Context) ->
    Limit  = undefined,
    Cursor = undefined,
    Filter = fun
        ({ID, {ev, Ts, {challenge, I, Body = {status_changed, _}}}}) when I =:= ChallengeId andalso ID =:= EventId ->
            {true, {ID, Ts, Body}};
        (_) ->
            false
    end,
    case do_get_identity_challenge_events(IdentityId, Limit, Cursor, Filter) of
        {ok, [Event]} -> {ok, Event};
        _             -> {error, notfound}
    end.

%% Wallets

-spec get_wallet(_, _) -> _.
get_wallet(WalletId, _Context) ->
    do(fun() -> to_swag(wallet, unwrap(ff_wallet_machine:get(WalletId))) end).

-spec create_wallet(_, _) -> _.
create_wallet(Params , Context) ->
    WalletId = genlib:unique(),
    case ff_wallet_machine:create(WalletId, from_swag(wallet_params, Params), make_ctx(Params, [])) of
        ok                 -> get_wallet(WalletId, Context);
        Error = {error, _} -> Error
    end.

-spec get_wallet_account(_, _) -> _.
get_wallet_account(WalletId, _Context) ->
    do(fun () ->
        {Amounts, Currency} = unwrap(ff_transaction:balance(
            unwrap(ff_wallet:account(ff_wallet_machine:wallet(unwrap(ff_wallet_machine:get(WalletId)))))
        )),
        to_swag(wallet_account, {ff_indef:current(Amounts), ff_indef:expmin(Amounts), Currency})
    end).

%% Withdrawals

-spec get_destinations(_, _) -> no_return().
get_destinations(_Params, _Context) ->
    not_implemented().

-spec get_destination(_, _) -> _.
get_destination(DestinationId, _Context) ->
    do(fun() -> to_swag(destination, unwrap(ff_destination_machine:get(DestinationId))) end).

-spec create_destination(_, _) -> _.
create_destination(Params, Context) ->
    DestinationId = genlib:unique(),
    case ff_destination_machine:create(DestinationId, from_swag(destination_params, Params), make_ctx(Params, [])) of
        ok                 -> get_destination(DestinationId, Context);
        Error = {error, _} -> Error
    end.

-spec create_withdrawal(_, _) -> _.
create_withdrawal(Params, Context) ->
    WithdrawalId = genlib:unique(),
    case ff_withdrawal_machine:create(WithdrawalId, from_swag(withdrawal_params, Params), make_ctx(Params, [])) of
        ok                 -> get_withdrawal(WithdrawalId, Context);
        Error = {error, _} -> Error
    end.

-spec get_withdrawal(_, _) -> _.
get_withdrawal(WithdrawalId, _Context) ->
    do(fun() -> to_swag(withdrawal, unwrap(ff_withdrawal_machine:get(WithdrawalId))) end).

-spec get_withdrawal_events(_, _) -> _.
get_withdrawal_events(Params = #{'withdrawalID' := WithdrawalId, 'limit' := Limit}, _Context) ->
    Cursor = genlib_map:get('eventCursor', Params),
    Filter = fun
        ({ID, {ev, Ts, Body = {status_changed, _}}}) ->
            {true, {ID, Ts, Body}};
        (_) ->
            false
    end,
    do_get_withdrawal_events(WithdrawalId, Limit, Cursor, Filter).

-spec get_withdrawal_event(_, _, _) -> _.
get_withdrawal_event(WithdrawalId, EventId, _Context) ->
    Limit  = undefined,
    Cursor = undefined,
    Filter = fun
        ({ID, {ev, Ts, Body = {status_changed, _}}}) when ID =:= EventId ->
            {true, {ID, Ts, Body}};
        (_) ->
            false
    end,
    case do_get_withdrawal_events(WithdrawalId, Limit, Cursor, Filter) of
        {ok, [Event]} -> {ok, Event};
        _             -> {error, notfound}
    end.

%% Residences

-spec get_residence(binary(), wctx()) -> result(map()).
get_residence(Residence, _Context) ->
    do(fun () ->
        to_swag(residence_object, unwrap(ff_residence:get(from_swag(residence, Residence))))
    end).

%% Currencies

-spec get_currency(binary(), wctx()) -> result(map()).
get_currency(CurrencyId, _Context) ->
    do(fun () ->
        to_swag(currency_object, unwrap(ff_currency:get(from_swag(currency, CurrencyId))))
    end).

%% Internal functions

do_get_withdrawal_events(WithdrawalId, Limit, Cursor, Filter) ->
    do(fun () ->
        _ = unwrap(ff_withdrawal_machine:get(WithdrawalId)),
        to_swag(withdrawal_events, collect_events(
            fun (C, L) ->
                unwrap(ff_withdrawal_machine:events(WithdrawalId, {C, L, forward}))
            end,
            Filter,
            Cursor,
            Limit
        ))
    end).

do_get_identity_challenge_events(IdentityId, Limit, Cursor, Filter) ->
    do(fun () ->
        _ = unwrap(ff_identity_machine:get(IdentityId)),
        to_swag(list, {identity_challenge_event, collect_events(
            fun (C, L) ->
                unwrap(ff_identity_machine:events(IdentityId, {C, L, forward}))
            end,
            Filter,
            Cursor,
            Limit
        )})
    end).

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

-define(CTX_NS, <<"com.rbkmoney.wapi">>).

make_ctx(Params, WapiKeys) ->
    Ctx0 = maps:with(WapiKeys, Params),
    Ctx1 = case maps:get(<<"metadata">>, Params, undefined) of
        undefined -> Ctx0;
        MD        -> Ctx0#{<<"md">> => MD}
    end,
    #{?CTX_NS => Ctx1}.

with_party(PartyId, Fun) ->
    try Fun()
    catch
        error:#'payproc_PartyNotFound'{} ->
            _ = ff_party:create(PartyId),
            Fun()
    end.

get_ctx(Ctx) ->
    unwrap(ff_ctx:get(?CTX_NS, Ctx)).

-spec not_implemented() -> no_return().
not_implemented() ->
    wapi_handler_utils:throw_not_implemented().

do(Fun) ->
    ff_pipeline:do(Fun).

unwrap(Res) ->
    ff_pipeline:unwrap(Res).

%% Marshalling
from_swag(identity_params, Params) ->
    #{
        party    => maps:get(<<"party">>   , Params),
        provider => maps:get(<<"provider">>, Params),
        class    => maps:get(<<"class">>   , Params)
    };
from_swag(identity_challenge_params, Params) ->
    #{
       class  => maps:get(<<"type">>, Params),
       proofs => from_swag(proofs, maps:get(<<"proofs">>, Params))
    };
from_swag(proofs, Proofs) ->
    from_swag(list, {proof, Proofs});
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
    #{
        identity => maps:get(<<"identity">>, Params),
        currency => maps:get(<<"currency">>, Params),
        name     => maps:get(<<"name">>    , Params)
    };
from_swag(destination_params, Params) ->
    #{
        identity => maps:get(<<"identity">>, Params),
        currency => maps:get(<<"currency">>, Params),
        name     => maps:get(<<"name">>    , Params),
        resource => from_swag(destination_resource, maps:get(<<"resource">>, Params))
    };
from_swag(destination_resource, #{
    <<"type">> := <<"BankCardDestinationResource">>,
    <<"token">> := WapiToken
}) ->
    BankCard = wapi_utils:base64url_to_map(WapiToken),
    {bank_card, #{
        token          => maps:get(<<"token">>, BankCard),
        payment_system => maps:get(<<"paymentSystem">>, BankCard),
        bin            => maps:get(<<"bin">>, BankCard),
        masked_pan     => maps:get(<<"maskedPan">>, BankCard)
    }};
from_swag(withdrawal_params, Params) ->
    #{
        source      => maps:get(<<"wallet">>     , Params),
        destination => maps:get(<<"destination">>, Params),
        body        => from_swag(withdrawal_body , maps:get(<<"body">>, Params))
    };
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

from_swag(list, {Type, List}) ->
    lists:map(fun(V) -> from_swag(Type, V) end, List).


to_swag(_, undefined) ->
    undefined;
to_swag(providers, Providers) ->
    to_swag(list, {provider, Providers});
to_swag(provider, Provider) ->
    to_swag(map, #{
       <<"id">> => ff_provider:id(Provider),
       <<"name">> => ff_provider:name(Provider),
       <<"residences">> => to_swag(list, {residence,
           ordsets:to_list(ff_provider:residences(Provider))
       })
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
    WapiCtx  = get_ctx(ff_identity_machine:ctx(State)),
    to_swag(map, #{
        <<"id">>                 => ff_identity:id(Identity),
        <<"name">>               => maps:get(<<"name">>, WapiCtx),
        <<"createdAt">>          => to_swag(timestamp, ff_identity_machine:created(State)),
        <<"provider">>           => ff_provider:id(ff_identity:provider(Identity)),
        <<"class">>              => ff_identity_class:id(ff_identity:class(Identity)),
        <<"level">>              => ff_identity_class:level_id(ff_identity:level(Identity)),
        <<"effectiveChallenge">> => to_swag(identity_effective_challenge, ff_identity:effective_challenge(Identity)),
        <<"isBlocked">>          => to_swag(is_blocked, ff_identity:is_accessible(Identity)),
        <<"metadata">>           => maps:get(<<"md">>, WapiCtx, undefined)
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
        <<"type">>          => ff_identity_class:challenge_class_id(ChallengeClass),
        <<"proofs">>        => Proofs
    }, to_swag(challenge_status, ff_identity_challenge:status(Challenge))));
to_swag(challenge_status, pending) ->
    #{<<"status">>  => <<"Pending">>};
to_swag(challenge_status, {completed, C = #{resolution := approved}}) ->
    to_swag(map, #{
        <<"status">>        => <<"Completed">>,
        <<"validUntil">>    => to_swag(timestamp, genlib_map:get(valid_until, C))
    });
to_swag(challenge_status, {completed, #{resolution := denied}}) ->
    #{
        <<"status">>        => <<"Failed">>,
        <<"failureReason">> => <<"Denied">>
    };
to_swag(challenge_status, {failed, Reason}) ->
    %% TODO
    %%  - Well, what if Reason is not scalar?
    #{
        <<"status">>        => <<"Failed">>,
        <<"failureReason">> => genlib:to_binary(Reason)
    };
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
to_swag(challenge_status, cancelled) ->
    #{<<"status">>      => <<"Cancelled">>};

to_swag(wallet, State) ->
    Wallet = ff_wallet_machine:wallet(State),
    to_swag(map, #{
        <<"id">>         => ff_wallet:id(Wallet),
        <<"name">>       => ff_wallet:name(Wallet),
        <<"createdAt">>  => to_swag(timestamp, ff_wallet_machine:created(State)),
        <<"isBlocked">>  => to_swag(is_blocked, ff_wallet:is_accessible(Wallet)),
        <<"identity">>   => ff_identity:id(ff_wallet:identity(Wallet)),
        <<"currency">>   => to_swag(currency, ff_wallet:currency(Wallet)),
        <<"metadata">>   => genlib_map:get(<<"md">>, get_ctx(ff_wallet_machine:ctx(State)))
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
    Destination = ff_destination_machine:destination(State),
    Wallet      = ff_destination:wallet(Destination),
    to_swag(map, maps:merge(
        #{
            <<"id">>         => ff_destination:id(Destination),
            <<"name">>       => ff_wallet:name(Wallet),
            <<"createdAt">>  => to_swag(timestamp, ff_destination_machine:created(State)),
            <<"isBlocked">>  => to_swag(is_blocked, ff_wallet:is_accessible(Wallet)),
            <<"identity">>   => ff_identity:id(ff_wallet:identity(Wallet)),
            <<"currency">>   => to_swag(currency, ff_wallet:currency(Wallet)),
            <<"resource">>   => to_swag(destination_resource, ff_destination:resource(Destination)),
            <<"metadata">>   => genlib_map:get(<<"md">>, get_ctx(ff_destination_machine:ctx(State)))
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
        <<"paymentSystem">> => genlib_map:get(payment_system, BankCard),
        <<"bin">>           => genlib_map:get(bin, BankCard),
        <<"lastDigits">>    => to_swag(pan_last_digits, genlib_map:get(masked_pan, BankCard))
    });
to_swag(pan_last_digits, MaskedPan) ->
    wapi_utils:get_last_pan_digits(MaskedPan);
to_swag(withdrawal, State) ->
    Withdrawal = ff_withdrawal_machine:withdrawal(State),
    to_swag(map, maps:merge(
        #{
            <<"id">>          => ff_withdrawal:id(Withdrawal),
            <<"createdAt">>   => to_swag(timestamp, ff_withdrawal_machine:created(State)),
            <<"metadata">>    => genlib_map:get(<<"md">>, get_ctx(ff_withdrawal_machine:ctx(State))),
            <<"wallet">>      => ff_wallet:id(ff_withdrawal:source(Withdrawal)),
            <<"destination">> => ff_destination:id(ff_withdrawal:destination(Withdrawal)),
            <<"body">>        => to_swag(withdrawal_body, ff_withdrawal:body(Withdrawal))
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
to_swag(withdrawal_status, {failed, Reason}) ->
    #{
        <<"status">> => <<"Failed">>,
        <<"failure">> => #{
            <<"code">> => genlib:to_binary(Reason)
        }
    };
to_swag(withdrawal_events, Events) ->
    to_swag(list, {withdrawal_event, Events});
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
    {ok, Timestamp} = rfc3339:format({Date, Time, Usec, undefined}),
    Timestamp;
to_swag(currency, Currency) ->
    genlib_string:to_upper(genlib:to_binary(Currency));
to_swag(currency_object, V) ->
    to_swag(map, #{
        <<"id">>          => to_swag(currency, maps:get(id, V)),
        <<"name">>        => maps:get(name, V),
        <<"numericCode">> => maps:get(numcode, V),
        <<"exponent">>    => maps:get(exponent, V),
        <<"sign">>        => maps:get(sign, V, undefined)
    });
to_swag(is_blocked, {ok, accessible}) ->
    false;
to_swag(is_blocked, _) ->
    true;

to_swag(list, {Type, List}) ->
    lists:map(fun(V) -> to_swag(Type, V) end, List);
to_swag(map, Map) ->
    genlib_map:compact(Map);
to_swag(_, V) ->
    V.
