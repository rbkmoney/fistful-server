-module(ff_eventsink_SUITE).

-include_lib("fistful_proto/include/ff_proto_withdrawal_thrift.hrl").

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
-export([get_withdrawal_session_events_ok/1]).
-export([get_create_destination_events_ok/1]).
-export([get_create_source_events_ok/1]).
-export([get_create_deposit_events_ok/1]).
-export([get_shifted_create_identity_events_ok/1]).
-export([get_create_p2p_transfer_events_ok/1]).
-export([get_create_w2w_transfer_events_ok/1]).
-export([get_create_p2p_template_events_ok/1]).

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: _ | no_return().

-spec all() -> [test_case_name()].

all() ->
    [
        get_identity_events_ok,
        get_create_wallet_events_ok,
        get_withdrawal_events_ok,
        get_create_destination_events_ok,
        get_create_source_events_ok,
        get_create_deposit_events_ok,
        get_withdrawal_session_events_ok,
        get_shifted_create_identity_events_ok,
        get_create_p2p_transfer_events_ok,
        get_create_w2w_transfer_events_ok,
        get_create_p2p_template_events_ok
    ].


-spec groups() -> [].

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
    ok = ct_helper:set_context(C1),
    C1.

-spec end_per_testcase(test_case_name(), config()) -> _.

end_per_testcase(_Name, _C) ->
    ok = ct_helper:unset_context().

%%

-spec get_identity_events_ok(config()) -> test_return().

get_identity_events_ok(C) ->
    ID = genlib:unique(),
    Party = create_party(C),
    Sink = identity_event_sink,
    LastEvent = ct_eventsink:last_id(Sink),

    ok = ff_identity_machine:create(
        #{
            id       => ID,
            party    => Party,
            provider => <<"good-one">>,
            class    => <<"person">>
        },
        ff_entity_context:new()
    ),
    ICID = genlib:unique(),
    D1 = ct_identdocstore:rus_retiree_insurance_cert(genlib:unique(), C),
    D2 = ct_identdocstore:rus_domestic_passport(C),
    ChallengeParams = #{
        id     => ICID,
        class  => <<"sword-initiation">>
    },
    ok = ff_identity_machine:start_challenge(
        ID, ChallengeParams#{proofs => [D1, D2]}
    ),
    {completed, _} = ct_helper:await(
        {completed, #{resolution => approved}},
        fun () ->
            {ok, S}  = ff_identity_machine:get(ID),
            {ok, IC} = ff_identity:challenge(ICID, ff_identity_machine:identity(S)),
            ff_identity_challenge:status(IC)
        end
    ),

    {ok, RawEvents} = ff_identity_machine:events(ID, {undefined, 1000, forward}),
    {_Events, MaxID} = ct_eventsink:events(LastEvent, 1000, Sink),
    MaxID = LastEvent + length(RawEvents).

-spec get_create_wallet_events_ok(config()) -> test_return().

get_create_wallet_events_ok(C) ->
    ID = genlib:unique(),
    Party = create_party(C),
    IdentityID = create_identity(Party, C),

    Sink = wallet_event_sink,
    LastEvent = ct_eventsink:last_id(Sink),

    ok = ff_wallet_machine:create(
        #{
            id       => ID,
            identity => IdentityID,
            name     => <<"EVENTS TEST">>,
            currency => <<"RUB">>
        },
        ff_entity_context:new()
    ),
    {ok, RawEvents} = ff_wallet_machine:events(ID, {undefined, 1000, forward}),
    {_Events, MaxID} = ct_eventsink:events(LastEvent, 1000, Sink),
    MaxID = LastEvent + length(RawEvents).

-spec get_withdrawal_events_ok(config()) -> test_return().

get_withdrawal_events_ok(C) ->
    Sink = withdrawal_event_sink,
    LastEvent = ct_eventsink:last_id(Sink),
    Party   = create_party(C),
    IID     = create_person_identity(Party, C),
    WalID   = create_wallet(IID, <<"HAHA NO2">>, <<"RUB">>, C),
    SrcID   = create_source(IID, C),
    _DepID  = process_deposit(SrcID, WalID),
    DestID  = create_destination(IID, C),
    WdrID   = process_withdrawal(WalID, DestID),

    {Events, MaxID} = ct_eventsink:events(LastEvent, 1000, Sink),
    {ok, RawEvents} = ff_withdrawal_machine:events(WdrID, {undefined, 1000}),

    AlienEvents = lists:filter(fun(Ev) ->
        Ev#wthd_SinkEvent.source =/= WdrID
    end, Events),

    MaxID = LastEvent + length(RawEvents) + length(AlienEvents).

-spec get_withdrawal_session_events_ok(config()) -> test_return().

get_withdrawal_session_events_ok(C) ->
    Sink = withdrawal_session_event_sink,
    LastEvent = ct_eventsink:last_id(Sink),

    Party   = create_party(C),
    IID     = create_person_identity(Party, C),
    WalID   = create_wallet(IID, <<"HAHA NO2">>, <<"RUB">>, C),
    SrcID   = create_source(IID, C),
    _DepID  = process_deposit(SrcID, WalID),
    DestID  = create_destination(IID, C),
    WdrID   = process_withdrawal(WalID, DestID),

    {ok, RawEvents} = ff_withdrawal_session_machine:events(
        WdrID,
        {undefined, 1000, forward}
    ),
    {_Events, MaxID} = ct_eventsink:events(LastEvent, 1000, Sink),
    MaxID = LastEvent + length(RawEvents).

-spec get_create_destination_events_ok(config()) -> test_return().

get_create_destination_events_ok(C) ->
    Sink = destination_event_sink,
    LastEvent = ct_eventsink:last_id(Sink),

    Party   = create_party(C),
    IID     = create_person_identity(Party, C),
    DestID = create_destination(IID, C),

    {ok, RawEvents} = ff_destination:events(DestID, {undefined, 1000, forward}),
    {_Events, MaxID} = ct_eventsink:events(LastEvent, 1000, Sink),
    MaxID = LastEvent + length(RawEvents).

-spec get_create_source_events_ok(config()) -> test_return().

get_create_source_events_ok(C) ->
    Sink = source_event_sink,
    LastEvent = ct_eventsink:last_id(Sink),

    Party   = create_party(C),
    IID     = create_person_identity(Party, C),
    SrcID   = create_source(IID, C),

    {ok, RawEvents} = ff_source:events(SrcID, {undefined, 1000, forward}),
    {_Events, MaxID} = ct_eventsink:events(LastEvent, 1000, Sink),
    MaxID = LastEvent + length(RawEvents).

-spec get_create_deposit_events_ok(config()) -> test_return().

get_create_deposit_events_ok(C) ->
    Sink = deposit_event_sink,
    LastEvent = ct_eventsink:last_id(Sink),

    Party   = create_party(C),
    IID     = create_person_identity(Party, C),
    WalID   = create_wallet(IID, <<"HAHA NO2">>, <<"RUB">>, C),
    SrcID   = create_source(IID, C),
    DepID   = process_deposit(SrcID, WalID),

    {ok, RawEvents} = ff_deposit_machine:events(DepID, {undefined, 1000}),
    {_Events, MaxID} = ct_eventsink:events(LastEvent, 1000, Sink),
    MaxID = LastEvent + length(RawEvents).

-spec get_shifted_create_identity_events_ok(config()) -> test_return().

get_shifted_create_identity_events_ok(C) ->
    #{suite_sup := SuiteSup} = ct_helper:cfg(payment_system, C),
    Service = identity_event_sink,
    StartEventNum = 3,
    IdentityRoute = create_sink_route(Service, {ff_eventsink_handler, #{
        ns          => <<"ff/identity">>,
        publisher   => ff_identity_eventsink_publisher,
        start_event => StartEventNum,
        schema      => machinery_mg_schema_generic
    }}),
    {ok, _} = supervisor:start_child(SuiteSup, woody_server:child_spec(
        ?MODULE,
        #{
            ip                => {0, 0, 0, 0},
            port              => 8040,
            handlers          => [],
            event_handler     => scoper_woody_event_handler,
            additional_routes => IdentityRoute
        }
    )),
    {ok, Events} = call_route_handler('GetEvents',
        Service, [#'evsink_EventRange'{'after' = 0, limit = 1}]),
    MaxID = ct_eventsink:get_max_event_id(Events),
    MaxID = StartEventNum + 1.

-spec get_create_p2p_transfer_events_ok(config()) -> test_return().

get_create_p2p_transfer_events_ok(C) ->
    ID = genlib:unique(),
    Party = create_party(C),
    IID = create_person_identity(Party, C),
    Sink = p2p_transfer_event_sink,
    LastEvent = ct_eventsink:last_id(Sink),

    Resource = {bank_card, #{bank_card => #{
        token => genlib:unique(),
        bin => <<"some bin">>,
        masked_pan => <<"some masked_pan">>
    }}},

    Participant = {raw, #{
        resource_params => Resource,
        contact_info => #{}
    }},

    Cash = {123, <<"RUB">>},

    CompactResource = {bank_card, #{
        token => genlib:unique(),
        bin_data_id => {binary, genlib:unique()}
    }},

    Quote = #{
        amount            => Cash,
        party_revision    => 1,
        domain_revision   => 1,
        created_at        => ff_time:now(),
        expires_on        => ff_time:now(),
        identity_id       => ID,
        sender            => CompactResource,
        receiver          => CompactResource
    },

    ok = p2p_transfer_machine:create(
        #{
            id => ID,
            identity_id => IID,
            body => Cash,
            sender => Participant,
            receiver => Participant,
            quote => Quote,
            client_info => #{
                ip_address => <<"some ip_address">>,
                fingerprint => <<"some fingerprint">>
            },
            external_id => ID
        },
        ff_entity_context:new()
    ),

    {ok, RawEvents} = p2p_transfer_machine:events(ID, {undefined, 1000, forward}),
    {_Events, MaxID} = ct_eventsink:events(LastEvent, 1000, Sink),
    MaxID = LastEvent + length(RawEvents).

-spec get_create_w2w_transfer_events_ok(config()) -> test_return().

get_create_w2w_transfer_events_ok(C) ->
    Sink = w2w_transfer_event_sink,
    LastEvent = ct_eventsink:last_id(Sink),

    Party = create_party(C),
    IID = create_person_identity(Party, C),
    WalFromID = create_wallet(IID, <<"HAHA NO1">>, <<"RUB">>, C),
    WalToID = create_wallet(IID, <<"HAHA NO2">>, <<"RUB">>, C),
    SrcID   = create_source(IID, C),
    _DepID  = process_deposit(SrcID, WalFromID),

    ID = process_w2w(WalFromID, WalToID),

    {ok, RawEvents} = w2w_transfer_machine:events(ID, {undefined, 1000}),
    {_Events, MaxID} = ct_eventsink:events(LastEvent, 1000, Sink),
    MaxID = LastEvent + length(RawEvents).

-spec get_create_p2p_template_events_ok(config()) -> test_return().

get_create_p2p_template_events_ok(C) ->
    Sink = p2p_template_event_sink,
    LastEvent = ct_eventsink:last_id(Sink),

    Party = create_party(C),
    IID = create_person_identity(Party, C),

    Details = make_template_details({1000, <<"RUB">>}),
    P2PTemplateID = generate_id(),
    P2PTemplateParams = #{
        id => P2PTemplateID,
        identity_id => IID,
        details => Details,
        external_id => P2PTemplateID
    },
    ok = p2p_template_machine:create(P2PTemplateParams, ff_entity_context:new()),

    {ok, RawEvents} = p2p_template_machine:events(P2PTemplateID, {undefined, 1000, forward}),
    {_Events, MaxID} = ct_eventsink:events(LastEvent, 1000, Sink),
    MaxID = LastEvent + length(RawEvents).

create_identity(Party, C) ->
    create_identity(Party, <<"good-one">>, <<"person">>, C).

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

create_person_identity(Party, C) ->
    create_identity(Party, <<"good-one">>, <<"person">>, C).

create_identity(Party, ProviderID, ClassID, _C) ->
    ID = genlib:unique(),
    ok = ff_identity_machine:create(
        #{id => ID, party => Party, provider => ProviderID, class => ClassID},
        ff_entity_context:new()
    ),
    ID.

create_wallet(IdentityID, Name, Currency, _C) ->
    ID = genlib:unique(),
    ok = ff_wallet_machine:create(
        #{id => ID, identity => IdentityID, name => Name, currency => Currency},
        ff_entity_context:new()
    ),
    ID.

create_instrument(Type, IdentityID, Name, Currency, Resource, C) ->
    ID = genlib:unique(),
    ok = create_instrument(
        Type,
        #{id => ID, identity => IdentityID, name => Name, currency => Currency, resource => Resource},
        ff_entity_context:new(),
        C
    ),
    ID.

create_instrument(destination, Params, Ctx, _C) ->
    ff_destination:create(Params, Ctx);
create_instrument(source, Params, Ctx, _C) ->
    ff_source:create(Params, Ctx).

generate_id() ->
    genlib:to_binary(genlib_time:ticks()).

create_source(IID, C) ->
    SrcResource = #{type => internal, details => <<"Infinite source of cash">>},
    SrcID = create_instrument(source, IID, <<"XSource">>, <<"RUB">>, SrcResource, C),
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
    ok = ff_deposit_machine:create(
        #{id => DepID, source_id => SrcID, wallet_id => WalID, body => {10000, <<"RUB">>}},
        ff_entity_context:new()
    ),
    succeeded = await_final_deposit_status(DepID),
    DepID.

create_destination(IID, C) ->
    DestResource = {bank_card, #{bank_card => ct_cardstore:bank_card(<<"4150399999000900">>, {12, 2025}, C)}},
    DestID = create_instrument(destination, IID, <<"XDesination">>, <<"RUB">>, DestResource, C),
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

    ok = ff_withdrawal_machine:create(
        #{id => WdrID, wallet_id => WalID, destination_id => DestID, body => {4240, <<"RUB">>}},
        ff_entity_context:new()
    ),
    succeeded = await_final_withdrawal_status(WdrID),
    true = ct_helper:await(
        true,
        fun () ->
            Sink = withdrawal_event_sink,
            {Events, _MaxID} = ct_eventsink:events(undefined, 1000, Sink),
            search_event_commited(Events, WdrID)
        end,
        genlib_retry:linear(15, 1000)
    ),
    WdrID.

process_w2w(WalletFromID, WalletToID) ->
    ID = generate_id(),
    ok = w2w_transfer_machine:create(
        #{id => ID, wallet_from_id => WalletFromID, wallet_to_id => WalletToID, body => {10000, <<"RUB">>}},
        ff_entity_context:new()
    ),
    succeeded = await_final_w2w_transfer_status(ID),
    ID.

get_w2w_transfer(DepositID) ->
    {ok, Machine} = w2w_transfer_machine:get(DepositID),
    w2w_transfer_machine:w2w_transfer(Machine).

get_w2w_transfer_status(DepositID) ->
    w2w_transfer:status(get_w2w_transfer(DepositID)).

await_final_w2w_transfer_status(DepositID) ->
    finished = ct_helper:await(
        finished,
        fun () ->
            {ok, Machine} = w2w_transfer_machine:get(DepositID),
            Deposit = w2w_transfer_machine:w2w_transfer(Machine),
            case w2w_transfer:is_finished(Deposit) of
                false ->
                    {not_finished, Deposit};
                true ->
                    finished
            end
        end,
        genlib_retry:linear(90, 1000)
    ),
    get_w2w_transfer_status(DepositID).

get_deposit(DepositID) ->
    {ok, Machine} = ff_deposit_machine:get(DepositID),
    ff_deposit_machine:deposit(Machine).

get_deposit_status(DepositID) ->
    ff_deposit:status(get_deposit(DepositID)).

await_final_deposit_status(DepositID) ->
    finished = ct_helper:await(
        finished,
        fun () ->
            {ok, Machine} = ff_deposit_machine:get(DepositID),
            Deposit = ff_deposit_machine:deposit(Machine),
            case ff_deposit:is_finished(Deposit) of
                false ->
                    {not_finished, Deposit};
                true ->
                    finished
            end
        end,
        genlib_retry:linear(90, 1000)
    ),
    get_deposit_status(DepositID).

get_withdrawal(WithdrawalID) ->
    {ok, Machine} = ff_withdrawal_machine:get(WithdrawalID),
    ff_withdrawal_machine:withdrawal(Machine).

get_withdrawal_status(WithdrawalID) ->
    ff_withdrawal:status(get_withdrawal(WithdrawalID)).

await_final_withdrawal_status(WithdrawalID) ->
    finished = ct_helper:await(
        finished,
        fun () ->
            {ok, Machine} = ff_withdrawal_machine:get(WithdrawalID),
            Withdrawal = ff_withdrawal_machine:withdrawal(Machine),
            case ff_withdrawal:is_finished(Withdrawal) of
                false ->
                    {not_finished, Withdrawal};
                true ->
                    finished
            end
        end,
        genlib_retry:linear(90, 1000)
    ),
    get_withdrawal_status(WithdrawalID).

call_route_handler(Function, ServiceName, Args) ->
    call_handler(Function, ServiceName, Args, <<"8040">>).

call_handler(Function, ServiceName, Args, Port) ->
    Service = ff_services:get_service(ServiceName),
    Path = erlang:list_to_binary(ff_services:get_service_path(ServiceName)),
    Request = {Service, Function, Args},
    Client  = ff_woody_client:new(#{
        url           => <<"http://localhost:", Port/binary, Path/binary>>,
        event_handler => scoper_woody_event_handler
    }),
    ff_woody_client:call(Client, Request).

create_sink_route(ServiceName, {Handler, Cfg}) ->
    Service = ff_services:get_service(ServiceName),
    Path = ff_services:get_service_path(ServiceName),
    NewCfg = Cfg#{
        client => #{
            event_handler => scoper_woody_event_handler,
            url => "http://machinegun:8022/v1/event_sink"
        }},
    PartyClient = party_client:create_client(),
    WrapperOptions = #{
        handler => {Handler, NewCfg},
        party_client => PartyClient
    },
    woody_server_thrift_http_handler:get_routes(genlib_map:compact(#{
        handlers => [{Path, {Service, {ff_woody_wrapper, WrapperOptions}}}],
        event_handler => scoper_woody_event_handler
    })).

search_event_commited(Events, WdrID) ->
    ClearEv = lists:filter(fun(Ev) ->
        case Ev#wthd_SinkEvent.source of
            WdrID -> true;
            _     -> false
        end
    end, Events),

    TransferCommited = lists:filter(fun(Ev) ->
        Payload = Ev#wthd_SinkEvent.payload,
        Changes = Payload#wthd_EventSinkPayload.changes,
        lists:any(fun is_commited_ev/1, Changes)
    end, ClearEv),

    length(TransferCommited) =/= 0.

is_commited_ev({transfer, #wthd_TransferChange{payload = TransferEvent}}) ->
    case TransferEvent of
        {status_changed, #transfer_StatusChange{status = {committed, #transfer_Committed{}}}} ->
            true;
        _Other ->
            false
    end;
is_commited_ev(_Other) ->
    false.

make_template_details({Amount, Currency}) ->
    make_template_details({Amount, Currency}, #{<<"test key">> => <<"test value">>}).

make_template_details({Amount, Currency}, Metadata) ->
    #{
        body => #{
            value => genlib_map:compact(#{
                amount => Amount,
                currency => Currency
            })
        },
        metadata => #{
            value => Metadata
        }
    }.
