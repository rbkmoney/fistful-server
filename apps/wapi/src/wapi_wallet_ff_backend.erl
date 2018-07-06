%% Temporary stab for wallet handler

-module(wapi_wallet_ff_backend).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
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
-export([get_identity_challenge_events/5]).
-export([get_identity_challenge_event/4]).

-export([get_destinations/2]).
-export([get_destination/2]).
-export([create_destination/2]).
-export([create_withdrawal/2]).
-export([get_withdrawal/2]).
-export([get_withdrawal_events/2]).
-export([get_withdrawal_event/3]).

-export([get_currency/2]).
-export([get_residence/2]).

%% Helper API
-export([not_implemented/0]).

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
    case ff_provider:get(ProviderId) of
        {ok, Provider}      -> {ok, to_swag(provider, {ProviderId, ff_provider:payinst(Provider)})};
        Error = {error, _}  -> Error
    end.

-spec get_provider_identity_classes(_, _) -> _.
get_provider_identity_classes(Id, _Context) ->
    case ff_provider:get(Id) of
        {ok, Provider} ->
            {ok, lists:map(
                fun(ClassId) ->
                    {ok, Class} = do_get_provider_identity_class(ClassId, Provider),
                    Class
                end,
                ff_provider:list_identity_classes(Provider)
            )};
        Error = {error, _} ->
            Error
    end.

-spec get_provider_identity_class(_, _, _) -> _.
get_provider_identity_class(ProviderId, ClassId, _Context) ->
    case ff_provider:get(ProviderId) of
        {ok, Provider}     -> do_get_provider_identity_class(ClassId, Provider);
        Error = {error, _} -> Error
    end.

do_get_provider_identity_class(ClassId, Provider) ->
    case ff_provider:get_identity_class(ClassId, Provider) of
        {ok, Class}        -> {ok, to_swag(identity_class, Class)};
        Error = {error, _} -> Error
    end.

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

-define(NS, <<"com.rbkmoney.wapi">>).

-spec get_identity(_, _) -> _.
get_identity(IdentityId, _Context) ->
    case ff_identity_machine:get(IdentityId) of
        {ok, IdentityState} ->
            {ok, to_swag(identity, IdentityState)};
        Error = {error, _} ->
            Error
    end.

-spec create_identity(_, _) -> _.
create_identity(Params = #{<<"party">> := PartyId}, Context) ->
    IdentityId = genlib:unique(),
    with_party(PartyId, fun() ->
        case ff_identity_machine:create(IdentityId, from_swag(identity_params, Params), make_ctx(Params, [<<"name">>])) of
            ok                 -> get_identity(IdentityId, Context);
            Error = {error, _} -> Error
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
    case get_identity(IdentityId, Context) of
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

-spec get_identity_challenge_events(binary(), binary(), undefined | integer(), pos_integer(), wctx()) ->
    {ok, [map()]} |
    {error, _}.
get_identity_challenge_events(Id, ChallengeId, Cursor, Limit, _Context) ->
    do(fun () ->
        _ = unwrap(ff_identity_machine:get(Id)),
        to_swag(
            {list, {event, challenge}},
            collect_events(
                fun (C, L) ->
                    unwrap(ff_identity_machine:events(Id, {C, L, forward}))
                end,
                fun
                    ({ID, {ev, Ts, {challenge, I, Body = {status_changed, _}}}}) when I =:= ChallengeId ->
                        {true, {ID, Ts, Body}};
                    (_) ->
                        false
                end,
                Cursor,
                Limit
            )
        )
    end).

collect_events(Collector, Filter, Cursor, Limit) ->
    collect_events(Collector, Filter, Cursor, Limit, []).

collect_events(Collector, Filter, Cursor, Limit, Acc) when Limit > 0 ->
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

-spec get_identity_challenge_event(_, _, _, _) -> no_return().
get_identity_challenge_event(_IdentityId, _ChallengeId, _EventId, _Context) ->
    not_implemented().

%% Withdrawals

-spec get_destinations(_, _) -> no_return().
get_destinations(_Params, _Context) ->
    not_implemented().

-spec get_destination(_, _) -> _.
get_destination(DestinationId, _Context) ->
    case ff_destination_machine:get(DestinationId) of
        {ok, DestinationState} -> {ok, to_swag(destination, DestinationState)};
        Error = {error, _}     -> Error
    end.

-spec create_destination(_, _) -> _.
create_destination(Params = #{<<"party">> := PartyId}, Context) ->
    DestinationId = genlib:unique(),
    with_party(PartyId, fun() ->
        case ff_destination_machine:create(
            DestinationId, from_swag(destination_params, Params), make_ctx(Params, [<<"name">>])
        ) of
            ok                 -> get_destination(DestinationId, Context);
            Error = {error, _} -> Error
        end
    end).

-spec create_withdrawal(_, _) -> _.
create_withdrawal(Params, Context) ->
    WithdrawalId = genlib:unique(),
    case ff_withdrawal_machine:create(WithdrawalId, from_swag(withdrawal_params, Params), make_ctx(Params, [])) of
        ok                 -> get_withdrawal(WithdrawalId, Context);
        Error = {error, _} -> Error
    end.

-spec get_withdrawal(_, _) -> _.
get_withdrawal(WithdrawalId, _Context) ->
    case ff_withdrawal_machine:get(WithdrawalId) of
        {ok, State}        -> {ok, to_swag(withdrawal, State)};
        Error = {error, _} -> Error
    end.

-spec get_withdrawal_events(_, _) -> _.
get_withdrawal_events(Params = #{'withdrawalID' := WithdrawalId, 'limit' := Limit}, _Context) ->
    case ff_withdrawal_machine:get_status_events(WithdrawalId, genlib_map:get('eventCursor', Params)) of
        {ok, Events}       -> {ok, to_swag(withdrawal_events, filter_status_events(Events, Limit))};
        Error = {error, _} -> Error
    end.

-spec get_withdrawal_event(_, _, _) -> _.
get_withdrawal_event(WithdrawalId, EventId, _Context) ->
    case ff_withdrawal_machine:get_status_events(WithdrawalId, undefined) of
        {ok, Events} ->
            case lists:keyfind(EventId, 1, filter_status_events(Events)) of
                false -> {error, {event, notfound}};
                Event -> {ok, to_swag(withdrawal_event, Event)}
            end;
        Error = {error, _} -> Error
    end.

-spec get_currency(binary(), wctx()) -> result(map()).
get_currency(CurrencyId, _Context) ->
    do(fun () ->
        to_swag(currency_object, unwrap(ff_currency:get(from_swag(currency, CurrencyId))))
    end).

-spec get_residence(binary(), wctx()) -> result(map()).
get_residence(Residence, _Context) ->
    do(fun () ->
        to_swag(residence_object, unwrap(ff_residence:get(from_swag(residence, Residence))))
    end).

%% Helper API

-spec not_implemented() -> no_return().
not_implemented() ->
    wapi_handler:throw_result(wapi_handler_utils:reply_error(501)).

%% Internal functions

do(Fun) ->
    ff_pipeline:do(Fun).

unwrap(Res) ->
    ff_pipeline:unwrap(Res).

make_ctx(Params, WapiKeys) ->
    Ctx0 = maps:with(WapiKeys, Params),
    Ctx1 = case maps:get(<<"metadata">>, Params, undefined) of
        undefined -> Ctx0;
        MD        -> Ctx0#{<<"md">> => MD}
    end,
    #{?NS => Ctx1}.

with_party(PartyId, Fun) ->
    try Fun()
    catch
        error:#'payproc_PartyNotFound'{} ->
            _ = ff_party:create(PartyId),
            Fun()
    end.

filter_status_events(Events) ->
    filter_status_events(Events, undefined).

filter_status_events(Events, Limit) ->
    filter_status_events(Events, [], Limit).

filter_status_events(_, Acc, Limit) when is_integer(Limit) andalso length(Acc) >= Limit ->
    Acc;
filter_status_events([], Acc, _) ->
    Acc;
filter_status_events([{ID, Ts, {created, _}} | Rest], Acc, Limit) ->
    filter_status_events(Rest, [{ID, Ts, undefined} | Acc], Limit);
filter_status_events([{ID, Ts, {status_changed, Status}} | Rest], Acc, Limit) ->
    filter_status_events(Rest, [{ID, Ts, Status} | Acc], Limit);
filter_status_events([_ | Rest], Acc, Limit) ->
    filter_status_events(Rest, Acc, Limit).

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
from_swag(proof_type, <<"RUSRetireeInsuranceCertificateData">>) ->
    rus_retiree_insurance_cert;
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
    #{<<"token">> := CdsToken} = wapi_utils:base64url_to_map(WapiToken),
    {bank_card, #{token => CdsToken}};
from_swag(withdrawal_params, Params) ->
    #{
        source      => maps:get(<<"wallet">>     , Params),
        destination => maps:get(<<"destination">>, Params),
        body        => from_swag(withdrawal_body , maps:get(<<"body">>, Params))
    };
from_swag(withdrawal_body, Body) ->
    {maps:get(<<"amount">>, Body), maps:get(<<"currency">>, Body)};
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
to_swag(provider, {Id, Provider}) ->
    to_swag(map, #{
       <<"id">> => Id,
       <<"name">> => Provider#'domain_PaymentInstitution'.name,
       <<"residences">> => to_swag(list, {residence,
           ordsets:to_list(Provider#'domain_PaymentInstitution'.residences)
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
to_swag(identity, #{identity := Identity, times := {CreatedAt, _}, ctx := Ctx}) ->
    {ok, WapiCtx}    = ff_ctx:get(?NS, Ctx),
    ProviderId       = ff_provider:id(ff_identity:provider(Identity)),
    #{id := ClassId} = ff_identity:class(Identity),
    to_swag(map, #{
        <<"id">>                 => ff_identity:id(Identity),
        <<"name">>               => maps:get(<<"name">>, WapiCtx),
        <<"metadata">>           => maps:get(<<"md">>, WapiCtx, undefined),
        <<"createdAt">>          => to_swag(timestamp, CreatedAt),
        <<"provider">>           => ProviderId,
        <<"class">>              => ClassId,
        <<"level">>              => ff_identity_class:level_id(ff_identity:level(Identity)),
        <<"effectiveChallenge">> => to_swag(identity_effective_challenge, ff_identity:effective_challenge(Identity)),
        <<"isBlocked">>          => to_swag(is_blocked, ff_identity:is_accessible(Identity))
    });
to_swag(identity_effective_challenge, {ok, ChallegeId}) ->
    ChallegeId;
to_swag(identity_effective_challenge, {error, notfound}) ->
    undefined;
to_swag(identity_challenge, {ChallengeId, Challenge, Proofs}) ->
    ChallengeClass = ff_identity_challenge:class(Challenge),
    maps:merge(
        to_swag(map, #{
            <<"id">>            => ChallengeId,
            % <<"createdAt">>     => <<"TODO">>,
            <<"level">>         => ff_identity_class:level_id(ff_identity_class:target_level(ChallengeClass)),
            <<"type">>          => ff_identity_class:challenge_class_id(ChallengeClass),
            <<"proofs">>        => Proofs
        }),
        to_swag(challenge_status,
            ff_identity_challenge:status(Challenge)
        )
    );
to_swag(challenge_status, pending) ->
    #{<<"status">>      => <<"Pending">>};
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
    % TODO
    %  - Well, what if Reason is not scalar?
    #{
    <<"status">>        => <<"Failed">>,
    <<"failureReason">> => genlib:to_binary(Reason)
    };
to_swag(challenge_status, cancelled) ->
    #{<<"status">>      => <<"Cancelled">>};
to_swag(destination, #{destination := Destination, times := {CreatedAt, _}, ctx := Ctx}) ->
    {ok, WapiCtx} = ff_ctx:get(?NS, Ctx),
    Wallet  = ff_destination:wallet(Destination),
    to_swag(map, #{
        <<"id">>         => ff_destination:id(Destination),
        <<"name">>       => maps:get(<<"name">>, WapiCtx),
        <<"metadata">>   => maps:get(<<"md">>, WapiCtx, undefined),
        <<"createdAt">>  => to_swag(timestamp, CreatedAt),
        %% TODO
        <<"isBlocked">>  => to_swag(is_blocked, {ok, accessible}), %% ff_destination:is_accessible(Destination)),
        <<"identity">>   => ff_identity:id(ff_wallet:identity(Wallet)),
        <<"currency">>   => to_swag(currency, ff_wallet:currency(Wallet)),
        <<"resource">>   => to_swag(destination_resource, ff_destination:resource(Destination)),
        <<"status">>     => to_swag(destination_status, ff_destination:status(Destination)),
        <<"validUntil">> => to_swag(destination_expiration, Destination)
    });
to_swag(destination_status, authorized) ->
    <<"Authorized">>;
to_swag(destination_status, unauthorized) ->
    <<"Unauthorized">>;
to_swag(destination_expiration, #{status := authorized, timeout := Timeout}) ->
    Timeout;
to_swag(destination_expiration, _) ->
    undefined;
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
to_swag(withdrawal, St = #{withdrawal := W, times := {CreatedAt, _}, ctx := Ctx}) ->
    {ok, WapiCtx} = ff_ctx:get(?NS, Ctx),
    Status = genlib_map:get(status, St),
    to_swag(map, #{
        <<"id">>          => ff_withdrawal:id(W),
        <<"createdAt">>   => to_swag(timestamp, CreatedAt),
        <<"metadata">>    => maps:get(<<"md">>, WapiCtx, undefined),
        <<"wallet">>      => ff_wallet:id(ff_withdrawal:source(W)),
        <<"destination">> => ff_destination:id(ff_withdrawal:destination(W)),
        <<"body">>        => to_swag(withdrawal_body, ff_withdrawal:body(W)),
        <<"status">>      => to_swag(withdrawal_status, Status),
        <<"failure">>     => to_swag(withdrawal_failure, Status)
    });
to_swag(withdrawal_body, Body) ->
    to_swag(map, #{
        <<"amount">>   => maps:get(amount, Body),
        <<"currency">> => to_swag(currency, maps:get(currency, Body))
    });
to_swag(withdrawal_status, succeeded) ->
    <<"Succeeded">>;
to_swag(withdrawal_status, failed) ->
    <<"Failed">>;
to_swag(withdrawal_status, {failed, _Reason}) ->
    <<"Failed">>;
to_swag(withdrawal_status, _) ->
    <<"Pending">>;
to_swag(withdrawal_failure, {failed, Reason}) ->
    genlib:to_binary(Reason);
to_swag(withdrawal_failure, _) ->
    undefined;
to_swag(withdrawal_events, Events) ->
    to_swag(list, {withdrawal_event, Events});
to_swag(withdrawal_event, {EventId, Ts, Status}) ->
    to_swag(map, #{
        <<"eventID">>   => EventId,
        <<"occuredAt">> => to_swag(timestamp, Ts),
        <<"changes">> => [#{
            <<"type">>    => <<"WithdrawalStatusChanged">>,
            <<"status">>  => to_swag(withdrawal_status, Status),
            <<"failure">> => to_swag(withdrawal_failure, Status)
        }]
    });
to_swag({event, Type}, {ID, Ts, V}) ->
    #{
        <<"eventID">>   => ID,
        <<"occuredAt">> => to_swag(timestamp, Ts),
        <<"changes">>   => [to_swag({change, Type}, V)]
    };
to_swag({change, challenge}, {status_changed, S}) ->
    maps:merge(
        #{<<"type">> => <<"IdentityChallengeStatusChanged">>},
        to_swag(challenge_status, S)
    );
to_swag(timestamp, {{Date, Time}, Usec}) ->
    rfc3339:format({Date, Time, Usec, undefined});
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

to_swag(_Type, V) when is_map(V) ->
    to_swag(map, V);
to_swag(list, {Type, List}) ->
    lists:map(fun(V) -> to_swag(Type, V) end, List);
to_swag(map, Map) ->
    genlib_map:compact(Map).
