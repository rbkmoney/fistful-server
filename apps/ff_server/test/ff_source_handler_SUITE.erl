-module(ff_source_handler_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("fistful_proto/include/ff_proto_source_thrift.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([get_source_events_ok_test/1]).
-export([get_source_context_ok_test/1]).
-export([create_source_ok_test/1]).
-export([unknown_test/1]).

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].

all() ->
    [{group, default}].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].

groups() ->
    [
        {default, [parallel], [
            get_source_events_ok_test,
            get_source_context_ok_test,
            create_source_ok_test,
            unknown_test
        ]}
    ].

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

-spec get_source_events_ok_test(config()) -> test_return().

get_source_events_ok_test(C) ->
    Resource = {internal, #src_Internal{
        details = <<"details">>
    }},
    State = create_source_ok(Resource, C),
    ID = State#src_SourceState.id,
    {ok, [_Event | _Rest]} = call_service('GetEvents', [ID, #'EventRange'{}]).

-spec get_source_context_ok_test(config()) -> test_return().

get_source_context_ok_test(C) ->
    Resource = {internal, #src_Internal{
        details = <<"details">>
    }},
    State = create_source_ok(Resource, C),
    ID = State#src_SourceState.id,
    {ok, _Context} = call_service('GetContext', [ID]).

-spec create_source_ok_test(config()) -> test_return().

create_source_ok_test(C) ->
    Resource = {internal, #src_Internal{
        details = <<"details">>
    }},
    create_source_ok(Resource, C).

-spec unknown_test(config()) -> test_return().
unknown_test(_C) ->
    ID = <<"unknown_id">>,
    Result = call_service('Get', [ID, #'EventRange'{}]),
    ExpectedError = #fistful_SourceNotFound{},
    ?assertEqual({exception, ExpectedError}, Result).

%%----------------------------------------------------------------------
%%  Internal functions
%%----------------------------------------------------------------------

create_source_ok(Resource, C) ->
    Party = create_party(C),
    Currency = <<"RUB">>,
    Name = <<"name">>,
    ID = genlib:unique(),
    ExternalId = genlib:unique(),
    IdentityID = create_person_identity(Party, C),
    Ctx = ff_entity_context_codec:marshal(#{<<"NS">> => #{}}),
    Metadata = ff_entity_context_codec:marshal(#{<<"metadata">> => #{<<"some key">> => <<"some data">>}}),
    Params = #src_SourceParams{
        id = ID,
        identity_id = IdentityID,
        name = Name,
        currency = #'CurrencyRef'{symbolic_code = Currency},
        resource = Resource,
        external_id = ExternalId,
        metadata = Metadata
    },
    {ok, Src} = call_service('Create', [Params, Ctx]),
    Name = Src#src_SourceState.name,
    ID = Src#src_SourceState.id,
    Resource = Src#src_SourceState.resource,
    ExternalId = Src#src_SourceState.external_id,
    Metadata = Src#src_SourceState.metadata,
    Ctx = Src#src_SourceState.context,

    Account = Src#src_SourceState.account,
    IdentityID = Account#account_Account.identity,
    #'CurrencyRef'{symbolic_code = Currency} = Account#account_Account.currency,

    {unauthorized, #src_Unauthorized{}} = Src#src_SourceState.status,

    {authorized, #src_Authorized{}} = ct_helper:await(
        {authorized, #src_Authorized{}},
        fun () ->
            {ok, #src_SourceState{status = Status}}
                = call_service('Get', [ID, #'EventRange'{}]),
            Status
        end,
        genlib_retry:linear(15, 1000)
    ),

    {ok, #src_SourceState{} = State} = call_service('Get', [ID, #'EventRange'{}]),
    State.

call_service(Fun, Args) ->
    Service = {ff_proto_source_thrift, 'Management'},
    Request = {Service, Fun, Args},
    Client  = ff_woody_client:new(#{
        url           => <<"http://localhost:8022/v1/source">>,
        event_handler => scoper_woody_event_handler
    }),
    ff_woody_client:call(Client, Request).


create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

create_person_identity(Party, C) ->
    create_identity(Party, <<"good-one">>, <<"person">>, C).

create_identity(Party, ProviderID, ClassID, C) ->
    create_identity(Party, <<"Identity Name">>, ProviderID, ClassID, C).

create_identity(Party, Name, ProviderID, ClassID, _C) ->
    ID = genlib:unique(),
    ok = ff_identity_machine:create(
        #{id => ID, name => Name, party => Party, provider => ProviderID, class => ClassID},
        #{<<"com.rbkmoney.wapi">> => #{<<"name">> => Name}}
    ),
    ID.
