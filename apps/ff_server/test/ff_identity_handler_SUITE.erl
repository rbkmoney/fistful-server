-module(ff_identity_handler_SUITE).

-include_lib("ff_cth/include/ct_domain.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([create_identity_ok/1]).
-export([get_event_unknown_identity_ok/1]).

-spec create_identity_ok(config()) -> test_return().
-spec get_event_unknown_identity_ok(config()) -> test_return().

%%

-type config() :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name() :: ct_helper:group_name().
-type test_return() :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [
        create_identity_ok,
        get_event_unknown_identity_ok
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    ct_helper:makeup_cfg(
        [
            ct_helper:test_case_name(init),
            ct_payment_system:setup()
        ],
        C
    ).

-spec end_per_suite(config()) -> _.
end_per_suite(C) ->
    ok = ct_payment_system:shutdown(C).

%%

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(Name, C) ->
    C1 = ct_helper:makeup_cfg([ct_helper:test_case_name(Name), ct_helper:woody_ctx()], C),
    ok = ct_helper:set_context(C1),
    C1.

-spec end_per_testcase(test_case_name(), config()) -> _.
end_per_testcase(_Name, _C) ->
    ok = ct_helper:unset_context().

%%-------
%% TESTS
%%-------

create_identity_ok(_C) ->
    PartyID = create_party(),
    EID = genlib:unique(),
    Name = <<"Identity Name">>,
    ProvID = ?IDENTITY_PROVIDER_NAME1,
    Ctx = #{<<"NS">> => #{<<"owner">> => PartyID}},
    Metadata = ff_entity_context_codec:marshal(#{<<"metadata">> => #{<<"some key">> => <<"some data">>}}),
    Identity = create_identity(EID, Name, PartyID, ProvID, Ctx, Metadata),
    IID = Identity#idnt_IdentityState.id,
    {ok, Identity_} = call_api('Get', {IID, #'EventRange'{}}),

    ProvID = Identity_#idnt_IdentityState.provider_id,
    IID = Identity_#idnt_IdentityState.id,
    Name = Identity_#idnt_IdentityState.name,
    PartyID = Identity_#idnt_IdentityState.party_id,
    unblocked = Identity_#idnt_IdentityState.blocking,
    Metadata = Identity_#idnt_IdentityState.metadata,
    Ctx0 = Ctx#{
        <<"com.rbkmoney.wapi">> => #{<<"name">> => Name}
    },
    Ctx0 = ff_entity_context_codec:unmarshal(Identity_#idnt_IdentityState.context),
    ok.

get_event_unknown_identity_ok(_C) ->
    Ctx = #{<<"NS">> => #{}},
    EID = genlib:unique(),
    PID = create_party(),
    Name = <<"Identity Name">>,
    ProvID = ?IDENTITY_PROVIDER_NAME1,
    Metadata = ff_entity_context_codec:marshal(#{<<"metadata">> => #{<<"some key">> => <<"some data">>}}),
    create_identity(EID, Name, PID, ProvID, Ctx, Metadata),
    Range = #'EventRange'{
        limit = 1,
        'after' = undefined
    },
    {exception, {fistful_IdentityNotFound}} = call_api('GetEvents', {<<"bad id">>, Range}).

%%----------
%% INTERNAL
%%----------

create_identity(EID, Name, PartyID, ProvID, Ctx, Metadata) ->
    Params = #idnt_IdentityParams{
        id = genlib:unique(),
        name = Name,
        party = PartyID,
        provider = ProvID,
        external_id = EID,
        metadata = Metadata
    },
    Context = ff_entity_context_codec:marshal(Ctx#{
        <<"com.rbkmoney.wapi">> => #{<<"name">> => Name}
    }),
    {ok, IdentityState} = call_api('Create', {Params, Context}),
    IdentityState.

call_api(Fun, Args) ->
    Service = {ff_proto_identity_thrift, 'Management'},
    Request = {Service, Fun, Args},
    Client = ff_woody_client:new(#{
        url => <<"http://localhost:8022/v1/identity">>,
        event_handler => scoper_woody_event_handler
    }),
    ff_woody_client:call(Client, Request).

create_party() ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.
