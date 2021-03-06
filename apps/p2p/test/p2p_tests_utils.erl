-module(p2p_tests_utils).

-include_lib("damsel/include/dmsl_accounter_thrift.hrl").

%% API
-export([
    prepare_standard_environment/1,
    prepare_standard_environment/2
]).

-type config() :: ct_helper:config().
-type token() :: tuple() | binary().
-type prepared_ids() :: #{
    identity_id => ff_identity:id(),
    party_id => ff_party:id(),
    sender => p2p_participant:participant(),
    receiver => p2p_participant:participant()
}.

-spec prepare_standard_environment(config()) -> prepared_ids().
prepare_standard_environment(C) ->
    prepare_standard_environment(undefined, C).

-spec prepare_standard_environment(token() | undefined, config()) -> prepared_ids().
prepare_standard_environment(Token, C) ->
    PartyID = create_party(C),
    IdentityID = create_person_identity(PartyID, C, <<"quote-owner">>),
    {ResourceSender, ResourceReceiver} =
        case Token of
            {missing, sender} ->
                {
                    create_resource_raw(<<"TEST_NOTFOUND_SENDER">>, C),
                    create_resource_raw(undefined, C)
                };
            {missing, receiver} ->
                {
                    create_resource_raw(undefined, C),
                    create_resource_raw(<<"TEST_NOTFOUND_RECEIVER">>, C)
                };
            {with_prefix, Prefix} ->
                TokenRandomised = generate_id(),
                TokenWithPrefix = <<Prefix/binary, TokenRandomised/binary>>,
                {
                    create_resource_raw(TokenWithPrefix, C),
                    create_resource_raw(TokenWithPrefix, C)
                };
            Other ->
                {create_resource_raw(Other, C), create_resource_raw(Other, C)}
        end,
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
    Resource =
        {bank_card, #{
            bank_card => NewStoreResource,
            auth_data =>
                {session, #{
                    session_id => <<"ID">>
                }}
        }},
    p2p_participant:create(raw, Resource, #{}).

create_person_identity(Party, C, ProviderID) ->
    create_identity(Party, ProviderID, <<"person">>, C).

create_identity(Party, ProviderID, ClassID, C) ->
    create_identity(Party, <<"Identity Name">>, ProviderID, ClassID, C).

create_identity(Party, Name, ProviderID, ClassID, _C) ->
    ID = genlib:unique(),
    ok = ff_identity_machine:create(
        #{id => ID, name => Name, party => Party, provider => ProviderID, class => ClassID},
        #{<<"com.rbkmoney.wapi">> => #{<<"name">> => Name}}
    ),
    ID.

generate_id() ->
    ff_id:generate_snowflake_id().

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.
