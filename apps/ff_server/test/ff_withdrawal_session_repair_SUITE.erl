-module(ff_withdrawal_session_repair_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("fistful_proto/include/ff_proto_withdrawal_session_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([repair_failed_session_with_success/1]).
-export([repair_failed_session_with_failure/1]).

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
            repair_failed_session_with_success,
            repair_failed_session_with_failure
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
    ok = ff_woody_ctx:set(ct_helper:get_woody_ctx(C1)),
    C1.

-spec end_per_testcase(test_case_name(), config()) -> _.

end_per_testcase(_Name, _C) ->
    ok = ff_woody_ctx:unset().

%% Tests

-spec repair_failed_session_with_success(config()) -> test_return().
repair_failed_session_with_success(C) ->
    PartyID = create_party(C),
    IdentityID = create_identity(PartyID, C),
    DestinationID = create_destination(IdentityID, C),
    SessionID = create_failed_session(IdentityID, DestinationID, C),
    ?assertEqual(active, get_session_status(SessionID)),
    timer:sleep(3000),
    ?assertEqual(active, get_session_status(SessionID)),
    {ok, ok} = call_repair([SessionID, {set_session_result, #wthd_session_SetResultRepair{
        result = {success, #wthd_session_SessionResultSuccess{
            trx_info = #'TransactionInfo'{
                id = SessionID,
                extra = #{}
            }
        }}
    }}]),
    Expected = {success, #domain_TransactionInfo{
        id = SessionID,
        extra = #{}
    }},
    ?assertMatch({finished, Expected}, get_session_status(SessionID)).

-spec repair_failed_session_with_failure(config()) -> test_return().
repair_failed_session_with_failure(C) ->
    PartyID = create_party(C),
    IdentityID = create_identity(PartyID, C),
    DestinationID = create_destination(IdentityID, C),
    SessionID = create_failed_session(IdentityID, DestinationID, C),
    ?assertEqual(active, get_session_status(SessionID)),
    timer:sleep(3000),
    ?assertEqual(active, get_session_status(SessionID)),
    {ok, ok} = call_repair([SessionID, {set_session_result, #wthd_session_SetResultRepair{
        result = {failed, #wthd_session_SessionResultFailed{
            failure = #'Failure'{
                code = SessionID
            }
        }}
    }}]),
    Expected = {failed, #domain_Failure{
        code = SessionID
    }},
    ?assertMatch({finished, Expected}, get_session_status(SessionID)).

%%  Internals

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

create_identity(Party, C) ->
    create_identity(Party, <<"good-one">>, <<"person">>, C).

create_identity(Party, ProviderID, ClassID, _C) ->
    ID = genlib:unique(),
    ok = ff_identity_machine:create(
        ID,
        #{party => Party, provider => ProviderID, class => ClassID},
        ff_ctx:new()
    ),
    ID.

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

create_failed_session(IdentityID, DestinationID, _C) ->
    ID = genlib:unique(),
    TransferData = #{
        id          => ID,
        cash        => invalid_cash,  % invalid cash
        sender      => IdentityID,
        receiver    => IdentityID
    },
    SessionParams = #{
        destination => DestinationID,
        provider_id => 1
    },
    ok = ff_withdrawal_session_machine:create(ID, TransferData, SessionParams),
    ID.

-spec get_session_status(machinery:id()) ->
    ff_withdrawal_session:status().
get_session_status(ID) ->
    {ok, SessionMachine} = ff_withdrawal_session_machine:get(ID),
    Session = ff_withdrawal_session_machine:session(SessionMachine),
    ff_withdrawal_session:status(Session).

call_repair(Args) ->
    Service = {ff_proto_withdrawal_session_thrift, 'Repairer'},
    Request = {Service, 'Repair', Args},
    Client  = ff_woody_client:new(#{
        url           => <<"http://localhost:8022/v1/repair/withdrawal/session">>,
        event_handler => scoper_woody_event_handler
    }),
    ff_woody_client:call(Client, Request).
