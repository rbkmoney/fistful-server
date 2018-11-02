-module(ff_eventsink_SUITE).

-include_lib("fistful_proto/include/ff_proto_fistful_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_wallet_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_withdrawal_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_eventsink_thrift.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([get_identity_events_ok/1]).
-export([get_create_wallet_events_ok/1]).
-export([get_withdrawal_events_ok/1]).

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: _ | no_return().

-type evsink_event() :: ff_proto_identity_thrift:'SinkEvent'() |
                        ff_proto_wallet_thrift:'SinkEvent'() |
                        ff_proto_withdrawal_thrift:'SinkEvent'().
-type evsink_id()    :: ff_proto_base_thrift:'EventID'().

-spec all() -> [test_case_name() | {group, group_name()}].

all() ->
    [
        get_identity_events_ok,
        get_create_wallet_events_ok,
        get_withdrawal_events_ok
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].

groups() -> [].

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    ct_helper:makeup_cfg([
        ct_helper:test_case_name(init),
        ct_payment_system:setup()
    ], C).

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    ok = ct_payment_system:shutdown(C).

%%

-spec init_per_group(group_name(), config()) -> config().

init_per_group(_, C) ->
    C.

-spec end_per_group(group_name(), config()) -> _.

end_per_group(_, _) ->
    ok.

%%

-spec init_per_testcase(test_case_name(), config()) -> config().

init_per_testcase(Name, C) ->
    C1 = ct_helper:makeup_cfg([ct_helper:test_case_name(Name), ct_helper:woody_ctx()], C),
    ok = ff_woody_ctx:set(ct_helper:get_woody_ctx(C1)),
    C1.

-spec end_per_testcase(test_case_name(), config()) -> _.

end_per_testcase(_Name, _C) ->
    ok = ff_woody_ctx:unset().

%%

-spec get_identity_events_ok(config()) -> test_return().

get_identity_events_ok(C) ->
    ID = genlib:unique(),
    Party = create_party(C),
    Service = {{ff_proto_identity_thrift, 'EventSink'}, <<"/v1/eventsink/identity">>},
    LastEvent = unwrap_last_sinkevent_id(
        call_eventsink_handler('GetLastEventID', Service, [])),

    ok = ff_identity_machine:create(
        ID,
        #{
            party    => Party,
            provider => <<"good-one">>,
            class    => <<"person">>
        },
        ff_ctx:new()
    ),
    ICID = genlib:unique(),
    {ok, S1} = ff_identity_machine:get(ID),
    I1 = ff_identity_machine:identity(S1),
    {error, notfound} = ff_identity:challenge(ICID, I1),
    D1 = ct_identdocstore:rus_retiree_insurance_cert(genlib:unique(), C),
    D2 = ct_identdocstore:rus_domestic_passport(C),
    ChallengeParams = #{
        id     => ICID,
        class  => <<"sword-initiation">>
    },
    {error, {challenge, {proof, insufficient}}} = ff_identity_machine:start_challenge(
        ID, ChallengeParams#{proofs => []}
    ),
    {error, {challenge, {proof, insufficient}}} = ff_identity_machine:start_challenge(
        ID, ChallengeParams#{proofs => [D1]}
    ),
    ok = ff_identity_machine:start_challenge(
        ID, ChallengeParams#{proofs => [D1, D2]}
    ),
    {error, {challenge, {pending, ICID}}} = ff_identity_machine:start_challenge(
        ID, ChallengeParams#{proofs => [D1, D2]}
    ),
    {ok, S2} = ff_identity_machine:get(ID),
    I2 = ff_identity_machine:identity(S2),
    {ok, IC1} = ff_identity:challenge(ICID, I2),
    pending = ff_identity_challenge:status(IC1),
    {completed, _} = ct_helper:await(
        {completed, #{resolution => approved}},
        fun () ->
            {ok, S}  = ff_identity_machine:get(ID),
            {ok, IC} = ff_identity:challenge(ICID, ff_identity_machine:identity(S)),
            ff_identity_challenge:status(IC)
        end
    ),

    {ok, RawEvents} = ff_identity_machine:events(ID, {undefined, 1000, forward}),
    {ok, Events} = call_eventsink_handler('GetEvents',
        Service, [#'evsink_EventRange'{'after' = LastEvent, limit = 1000}]),
    MaxID    = get_max_sinkevent_sequence(Events),
    MaxID    = LastEvent + length(RawEvents).

-spec get_create_wallet_events_ok(config()) -> test_return().

get_create_wallet_events_ok(C) ->
    ID = genlib:unique(),
    Party = create_party(C),
    IdentityID = create_identity(Party, C),

    Service = {{ff_proto_wallet_thrift, 'EventSink'}, <<"/v1/eventsink/wallet">>},
    LastEvent = unwrap_last_sinkevent_id(
        call_eventsink_handler('GetLastEventID', Service, [])),

    ok = ff_wallet_machine:create(
        ID,
        #{
            identity => IdentityID,
            name     => <<"EVENTS TEST">>,
            currency => <<"RUB">>
        },
        ff_ctx:new()
    ),
    {ok, RawEvents} = ff_wallet_machine:events(ID, {undefined, 1000, forward}),
    {ok, Events} = call_eventsink_handler('GetEvents',
        Service, [#'evsink_EventRange'{'after' = LastEvent, limit = 1000}]),
    MaxID    = get_max_sinkevent_sequence(Events),
    MaxID    = LastEvent + length(RawEvents).

-spec get_withdrawal_events_ok(config()) -> test_return().

get_withdrawal_events_ok(C) ->
    Service = {{ff_proto_withdrawal_thrift, 'EventSink'}, <<"/v1/eventsink/withdrawal">>},
    LastEvent = unwrap_last_sinkevent_id(
        call_eventsink_handler('GetLastEventID', Service, [])),

    Party   = create_party(C),
    IID     = create_person_identity(Party, C),
    WalID   = create_wallet(IID, <<"HAHA NO2">>, <<"RUB">>, C),
    SrcID   = create_source(IID, C),
    succeeded = process_deposit(SrcID, WalID),
    DestID  = create_destination(IID, C),
    WdrID   = process_withdrawal(WalID, DestID),

    {ok, RawEvents} = ff_withdrawal:events(WdrID, {undefined, 1000, forward}),
    {ok, Events} = call_eventsink_handler('GetEvents',
        Service, [#'evsink_EventRange'{'after' = LastEvent, limit = 1000}]),
    MaxID    = get_max_sinkevent_sequence(Events),
    MaxID    = LastEvent + length(RawEvents).

create_identity(Party, C) ->
    create_identity(Party, <<"good-one">>, <<"person">>, C).

create_party(_C) ->
    ID = genlib:unique(),
    _ = ff_party:create(ID),
    ID.

create_person_identity(Party, C) ->
    create_identity(Party, <<"good-one">>, <<"person">>, C).

create_identity(Party, ProviderID, ClassID, _C) ->
    ID = genlib:unique(),
    ok = ff_identity_machine:create(
        ID,
        #{party => Party, provider => ProviderID, class => ClassID},
        ff_ctx:new()
    ),
    ID.

create_wallet(IdentityID, Name, Currency, _C) ->
    ID = genlib:unique(),
    ok = ff_wallet_machine:create(
        ID,
        #{identity => IdentityID, name => Name, currency => Currency},
        ff_ctx:new()
    ),
    ID.

create_instrument(Type, IdentityID, Name, Currency, Resource, C) ->
    ID = genlib:unique(),
    ok = create_instrument(
        Type,
        ID,
        #{identity => IdentityID, name => Name, currency => Currency, resource => Resource},
        ff_ctx:new(),
        C
    ),
    ID.

create_instrument(destination, ID, Params, Ctx, _C) ->
    ff_destination:create(ID, Params, Ctx);
create_instrument(source, ID, Params, Ctx, _C) ->
    ff_source:create(ID, Params, Ctx).

generate_id() ->
    genlib:to_binary(genlib_time:ticks()).

create_source(IID, C) ->
    SrcResource = #{type => internal, details => <<"Infinite source of cash">>},
    SrcID = create_instrument(source, IID, <<"XSource">>, <<"RUB">>, SrcResource, C),
    {ok, SrcM1} = ff_source:get_machine(SrcID),
    Src1 = ff_source:get(SrcM1),
    unauthorized = ff_source:status(Src1),
    authorized = ct_helper:await(
        authorized,
        fun () ->
            {ok, SrcM} = ff_source:get_machine(SrcID),
            ff_source:status(ff_source:get(SrcM))
        end
    ),
    SrcID.

process_deposit(SrcID, WalID) ->
    DepID = generate_id(),
    ok = ff_deposit:create(
        DepID,
        #{source_id => SrcID, wallet_id => WalID, body => {10000, <<"RUB">>}},
        ff_ctx:new()
    ),
    {ok, DepM1} = ff_deposit:get_machine(DepID),
    pending = ff_deposit:status(ff_deposit:get(DepM1)),
    succeeded = ct_helper:await(
        succeeded,
        fun () ->
            {ok, DepM} = ff_deposit:get_machine(DepID),
            ff_deposit:status(ff_deposit:get(DepM))
        end,
        genlib_retry:linear(15, 1000)
    ).

create_destination(IID, C) ->
    DestResource = {bank_card, ct_cardstore:bank_card(<<"4150399999000900">>, {12, 2025}, C)},
    DestID = create_instrument(destination, IID, <<"XDesination">>, <<"RUB">>, DestResource, C),
    {ok, DestM1} = ff_destination:get_machine(DestID),
    Dest1 = ff_destination:get(DestM1),
    unauthorized = ff_destination:status(Dest1),
    authorized = ct_helper:await(
        authorized,
        fun () ->
            {ok, DestM} = ff_destination:get_machine(DestID),
            ff_destination:status(ff_destination:get(DestM))
        end
    ),
    DestID.

process_withdrawal(WalID, DestID) ->
    WdrID = generate_id(),
    ok = ff_withdrawal:create(
        WdrID,
        #{wallet_id => WalID, destination_id => DestID, body => {4240, <<"RUB">>}},
        ff_ctx:new()
    ),
    {ok, WdrM1} = ff_withdrawal:get_machine(WdrID),
    pending = ff_withdrawal:status(ff_withdrawal:get(WdrM1)),
    succeeded = ct_helper:await(
        succeeded,
        fun () ->
            {ok, WdrM} = ff_withdrawal:get_machine(WdrID),
            ff_withdrawal:status(ff_withdrawal:get(WdrM))
        end,
        genlib_retry:linear(15, 1000)
    ),
    WdrID.


-spec get_max_sinkevent_sequence(list(evsink_event())) -> evsink_id().

get_max_sinkevent_sequence(Events) when is_list(Events) ->
    lists:foldl(fun (Ev, Max) -> erlang:max(get_sinkevent_sequence(Ev), Max) end, 0, Events).

get_sinkevent_sequence(#'wlt_SinkEvent'{sequence = Sequence}) -> Sequence;
get_sinkevent_sequence(#'wthd_SinkEvent'{sequence = Sequence}) -> Sequence;
get_sinkevent_sequence(#'idnt_SinkEvent'{sequence = Sequence}) -> Sequence.

-spec unwrap_last_sinkevent_id({ok | error, evsink_id()}) -> evsink_id().

unwrap_last_sinkevent_id({ok, EventID}) ->
    EventID;
unwrap_last_sinkevent_id({exception, #'evsink_NoLastEvent'{}}) ->
    0.

-spec call_eventsink_handler(atom(), tuple(), list()) ->
    {ok, woody:result()} |
    {exception, woody_error:business_error()}.

call_eventsink_handler(Function, {Service, Path}, Args) ->
    Request = {Service, Function, Args},
    Client  = ff_woody_client:new(#{
        url           => <<"http://localhost:8022", Path/binary>>,
        event_handler => scoper_woody_event_handler
    }),
    ff_woody_client:call(Client, Request).
