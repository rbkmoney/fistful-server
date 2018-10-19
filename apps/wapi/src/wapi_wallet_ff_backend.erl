-module(wapi_wallet_ff_backend).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

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

-export([get_destinations/2]).
-export([get_destination/2]).
-export([create_destination/2]).
-export([create_withdrawal/2]).
-export([get_withdrawal/2]).
-export([get_withdrawal_events/2]).
-export([get_withdrawal_event/3]).

-export([get_residence/2]).
-export([get_currency/2]).

%% Types

-type ctx()         :: wapi_handler:context().
-type params()      :: map().
-type id()          :: binary().
-type result()      :: result(map()).
-type result(T)     :: result(T, notfound).
-type result(T, E)  :: {ok, T} | {error, E}.

-define(CTX_NS, <<"com.rbkmoney.wapi">>).

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
    {email, notfound}
).
create_identity(Params, Context) ->
    IdentityId = next_id('identity'),
    do(fun() ->
        with_party(Context, fun() ->
            ok = unwrap(ff_identity_machine:create(
                IdentityId,
                maps:merge(from_swag(identity_params, Params), #{party => wapi_handler_utils:get_owner(Context)}),
                make_ctx(Params, [<<"name">>], Context
            ))),
            ok = scoper:add_meta(#{identity_id => IdentityId}),
            ok = lager:info("Identity created"),
            unwrap(get_identity(IdentityId, Context))
        end)
    end).

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
    ChallengeId = next_id('identity-challenge'),
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
get_wallet(WalletId, Context) ->
    do(fun() -> to_swag(wallet, get_state(wallet, WalletId, Context)) end).

-spec create_wallet(params(), ctx()) -> result(map(),
    invalid                  |
    {identity, unauthorized} |
    {identity, notfound}     |
    {currency, notfound}     |
    {inaccessible, _}
).
create_wallet(Params = #{<<"identity">> := IdenityId}, Context) ->
    WalletId = next_id('wallet'),
    do(fun() ->
        _ = check_resource(identity, IdenityId, Context),
        ok = unwrap(
            ff_wallet_machine:create(WalletId, from_swag(wallet_params, Params), make_ctx(Params, [], Context))
        ),
        unwrap(get_wallet(WalletId, Context))
    end).

-spec get_wallet_account(id(), ctx()) -> result(map(),
    {wallet, notfound}     |
    {wallet, unauthorized}
).
get_wallet_account(WalletId, Context) ->
    do(fun () ->
        Account = ff_wallet:account(ff_wallet_machine:wallet(get_state(wallet, WalletId, Context))),
        {Amounts, Currency} = unwrap(ff_transaction:balance(
            ff_account:accounter_account_id(Account)
        )),
        to_swag(wallet_account, {ff_indef:current(Amounts), ff_indef:expmin(Amounts), Currency})
    end).

%% Withdrawals

-spec get_destinations(params(), ctx()) -> no_return().
get_destinations(_Params, _Context) ->
    not_implemented().

-spec get_destination(id(), ctx()) -> result(map(),
    {destination, notfound}     |
    {destination, unauthorized}
).
get_destination(DestinationId, Context) ->
    do(fun() -> to_swag(destination, get_state(destination, DestinationId, Context)) end).

-spec create_destination(params(), ctx()) -> result(map(),
    invalid                  |
    {identity, unauthorized} |
    {identity, notfound}     |
    {currency, notfound}     |
    {inaccessible, _}
).
create_destination(Params = #{<<"identity">> := IdenityId}, Context) ->
    DestinationId = next_id('destination'),
    do(fun() ->
        _ = check_resource(identity, IdenityId, Context),
        ok = unwrap(ff_destination:create(
            DestinationId, from_swag(destination_params, Params), make_ctx(Params, [], Context)
        )),
        unwrap(get_destination(DestinationId, Context))
    end).

-spec create_withdrawal(params(), ctx()) -> result(map(),
    {source, notfound}            |
    {destination, notfound}       |
    {destination, unauthorized}   |
    {provider, notfound}          |
    {wallet, {inaccessible, _}}   |
    {wallet, {currency, invalid}} |
    {wallet, {provider, invalid}}
).
create_withdrawal(Params, Context) ->
    WithdrawalId = next_id('withdrawal'),
    do(fun() ->
        ok = unwrap(ff_withdrawal:create(
            WithdrawalId, from_swag(withdrawal_params, Params), make_ctx(Params, [], Context)
        )),
        unwrap(get_withdrawal(WithdrawalId, Context))
    end).

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

%% Internal functions

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

make_ctx(Params, WapiKeys, Context) ->
    #{?CTX_NS => maps:merge(
        #{<<"owner">> => wapi_handler_utils:get_owner(Context)},
        maps:with([<<"metadata">> | WapiKeys], Params)
    )}.

get_ctx(State) ->
    unwrap(ff_ctx:get(?CTX_NS, ff_machine:ctx(State))).

get_resource_owner(State) ->
    maps:get(<<"owner">>, get_ctx(State)).

is_resource_owner(HandlerCtx, State) ->
    wapi_handler_utils:get_owner(HandlerCtx) =:= get_resource_owner(State).

check_resource_access(HandlerCtx, State) ->
    check_resource_access(is_resource_owner(HandlerCtx, State)).

check_resource_access(true)  -> ok;
check_resource_access(false) -> {error, unauthorized}.

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

%% ID Gen
next_id(Type) ->
    NS = 'ff/sequence',
    erlang:integer_to_binary(
        ff_sequence:next(NS, ff_string:join($/, [Type, id]), fistful:backend(NS))
    ).

%% Marshalling

-type swag_term() ::
    #{binary() => swag_term()} |
    [swag_term()]              |
    number()                   |
    binary()                   |
    boolean()                  .

-spec from_swag(_Type, swag_term()) ->
    _Term.

from_swag(identity_params, Params) ->
    #{
        provider => maps:get(<<"provider">>, Params),
        class    => maps:get(<<"class">>   , Params)
    };
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
        payment_system => erlang:binary_to_existing_atom(maps:get(<<"paymentSystem">>, BankCard), latin1),
        bin            => maps:get(<<"bin">>, BankCard),
        masked_pan     => maps:get(<<"lastDigits">>, BankCard)
    }};
from_swag(withdrawal_params, Params) ->
    #{
        source      => maps:get(<<"wallet">>     , Params),
        destination => maps:get(<<"destination">>, Params),
        body        => from_swag(withdrawal_body , maps:get(<<"body">>, Params))
    };
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
        <<"metadata">>           => maps:get(<<"metadata">>, WapiCtx, undefined)
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
    to_swag(map, #{
        <<"id">>         => ff_wallet:id(Wallet),
        <<"name">>       => ff_wallet:name(Wallet),
        <<"createdAt">>  => to_swag(timestamp, ff_machine:created(State)),
        <<"isBlocked">>  => to_swag(is_blocked, ff_wallet:is_accessible(Wallet)),
        <<"identity">>   => ff_wallet:identity(Wallet),
        <<"currency">>   => to_swag(currency, ff_wallet:currency(Wallet)),
        <<"metadata">>   => genlib_map:get(<<"metadata">>, get_ctx(State))
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
            <<"metadata">>   => genlib_map:get(<<"metadata">>, get_ctx(State))
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
to_swag(pan_last_digits, MaskedPan) ->
    wapi_utils:get_last_pan_digits(MaskedPan);
to_swag(withdrawal, State) ->
    Withdrawal = ff_withdrawal:get(State),
    to_swag(map, maps:merge(
        #{
            <<"id">>          => ff_withdrawal:id(Withdrawal),
            <<"createdAt">>   => to_swag(timestamp, ff_machine:created(State)),
            <<"metadata">>    => genlib_map:get(<<"metadata">>, get_ctx(State)),
            <<"wallet">>      => ff_withdrawal:wallet_id(Withdrawal),
            <<"destination">> => ff_withdrawal:destination_id(Withdrawal),
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
    genlib:to_binary(Failure);
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

to_swag({list, Type}, List) ->
    lists:map(fun(V) -> to_swag(Type, V) end, List);
to_swag(map, Map) ->
    genlib_map:compact(Map);
to_swag(_, V) ->
    V.
