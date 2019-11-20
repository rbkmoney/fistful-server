-module(p2p_tests_utils).

-include_lib("damsel/include/dmsl_accounter_thrift.hrl").

%% API
-export([
    prepare_standard_environment/2,
    prepare_standard_environment/3
]).

-type config() :: ct_helper:config().
-type cash() :: ff_cash:cash().
-type token() :: binary().
-type prepared_ids() :: #{
    identity_id => ff_identity:id(),
    party_id => ff_party:id(),
    sender => p2p_participant:participant(),
    receiver => p2p_participant:participant()
}.

-spec prepare_standard_environment(cash(), config()) -> prepared_ids().

prepare_standard_environment(P2PTransferCash, C) ->
    prepare_standard_environment(P2PTransferCash, undefined, C).

-spec prepare_standard_environment(cash(), token(), config()) -> prepared_ids().

prepare_standard_environment(_P2PTransferCash, Token, C) ->
    PartyID = create_party(C),
    IdentityID = create_person_identity(PartyID, C, <<"quote-owner">>),
    {ResourceSender, ResourceReceiver} =
        case Token of
            <<"TEST_NOTFOUND_SENDER">> = T -> {
                create_resource_raw(T, C),
                create_resource_raw(undefined, C)
            };
            <<"TEST_NOTFOUND_RECEIVER">> = T -> {
                create_resource_raw(undefined, C),
                create_resource_raw(T, C)
            };
            Other -> {create_resource_raw(Other, C), create_resource_raw(Other, C)}
        end,
    % ct:print(" >>> ~p", [{ResourceSender, ResourceReceiver}]),
    #{
        identity_id => IdentityID,
        sender => ResourceSender,
        receiver => ResourceReceiver,
        party_id => PartyID
    }.

create_resource_raw(Token, C) ->
    StoreSource = ct_cardstore:bank_card(<<"4150399999000900">>, {12, 2025}, C),
    NewStoreResource =
        case Token of
            undefined ->
                StoreSource;
            Token ->
                StoreSource#{token => Token}
        end,
    p2p_participant:create(raw, {bank_card, NewStoreResource}).

create_person_identity(Party, C, ProviderID) ->
    create_identity(Party, ProviderID, <<"person">>, C).

create_identity(Party, ProviderID, ClassID, _C) ->
    ID = genlib:unique(),
    ok = ff_identity_machine:create(
        ID,
        #{party => Party, provider => ProviderID, class => ClassID},
        ff_entity_context:new()
    ),
    ID.

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.
