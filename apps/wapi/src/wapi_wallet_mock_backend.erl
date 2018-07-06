%% Temporary stab for wallet handler

-module(wapi_wallet_mock_backend).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

-export([get_providers/2]).
-export([get_provider/2]).
-export([get_provider_identity_classes/2]).
-export([get_provider_identity_class/3]).
-export([get_provider_identity_class_levels/3]).
-export([get_provider_identity_class_level/4]).

-export([get_identities/2]).
-export([get_identity/2]).
-export([create_identity/2]).

-export([get_destinations/2]).
-export([get_destination/2]).
-export([create_destination/2]).
-export([create_withdrawal/2]).
-export([get_withdrawal/2]).
-export([get_withdrawal_events/2]).
-export([get_withdrawal_event/3]).

%% API

-spec get_providers(_, _) -> _.
get_providers(_Params, _Context) ->
    {ok, [#{
        <<"id">>       => <<"1">>,
        <<"name">>     => <<"НКО «ЭПС»">>,
        <<"residences">> => [<<"RUS">>]
    }]}.

-spec get_provider(_, _) -> _.
get_provider(_Id, _Context) ->
    {ok, #{
        <<"id">>       => <<"1">>,
        <<"name">>     => <<"НКО «ЭПС»">>,
        <<"residences">> => [<<"RUS">>]
    }}.

-spec get_provider_identity_classes(_, _) -> _.
get_provider_identity_classes(_Id, _Context) ->
    {ok, [#{
        <<"id">>   => <<"person">>,
        <<"name">> => <<"Частная харя">>
    }]}.

-spec get_provider_identity_class(_, _, _) -> _.
get_provider_identity_class(_ProviderId, _ClassId, _Context) ->
    {ok, #{id => <<"person">>, name => <<"Частная харя">>}}.

-spec get_provider_identity_class_levels(_, _, _) -> _.
get_provider_identity_class_levels(_ProviderId, _ClassId, _Context) ->
    {ok, [
        #{
            <<"id">>   => <<"partial">>,
            <<"name">> => <<"Частично идентифицирован(а/о)">>,
            <<"challenges">> => #{
                <<"id">>   => <<"esia">>,
                <<"name">> => <<"Упрощённая идентификация">>,
                <<"requiredProofs">> => [
                    <<"RUSDomesticPassport">>,
                    <<"RUSRetireeInsuranceCertificate">>
                ]
            }
        },
        #{
            <<"id">>   => <<"full">>,
            <<"name">> => <<"Полностью идентифицирован(а/о)">>,
            <<"challenges">> => #{
                <<"id">>   => <<"svyaznoi bpa">>,
                <<"name">> => <<"Полная идентификацияв Связном">>,
                <<"requiredProofs">> => [
                    <<"RUSDomesticPassport">>,
                    <<"RUSRetireeInsuranceCertificate">>
                ]
            }
        }
    ]}.

-spec get_provider_identity_class_level(_, _, _, _) -> _.
get_provider_identity_class_level(_ProviderId, _ClassId, _LevelId, _Context) ->
    {ok, #{
        <<"id">>   => <<"partial">>,
        <<"name">> => <<"Частично идентифицирован(а/о)">>,
        <<"challenges">> => #{
            <<"id">>   => <<"esia">>,
            <<"name">> => <<"Упрощённая идентификация">>,
            <<"requiredProofs">> => [
                <<"RUSDomesticPassport">>,
                <<"RUSRetireeInsuranceCertificate">>
            ]
        }
    }}.

-spec get_identities(_, _) -> _.
get_identities(_Params, _Context) ->
    {ok, [#{
        <<"id">>                 => <<"douknowdawae">>,
        <<"name">>               => <<"Keyn Fawkes aka Slug">>,
        <<"metadata">>           => #{<<"is real">> => false},
        <<"createdAt">>          => {{{1989, 01, 17}, {12, 01, 45}}, 0},
        <<"provider">>           => <<"1">>,
        <<"class">>              => <<"person">>,
        <<"level">>              => <<"partial">>,
        <<"effectiveChallenge">> => <<"25">>,
        <<"isBlocked">>          => false
    }]}.

-spec get_identity(_, _) -> _.
get_identity(IdentityId, _Context) ->
    {ok, #{
        <<"id">>                 => IdentityId,
        <<"name">>               => <<"Keyn Fawkes aka Slug">>,
        <<"metadata">>           => #{<<"is real">> => false},
        <<"createdAt">>          => {{{1989, 01, 17}, {12, 01, 45}}, 0},
        <<"provider">>           => <<"1">>,
        <<"class">>              => <<"person">>,
        <<"level">>              => <<"partial">>,
        <<"effectiveChallenge">> => <<"25">>,
        <<"isBlocked">>          => false
    }}.

-spec create_identity(_, _) -> _.
create_identity(_Params, Context) ->
    get_identity(woody_context:new_req_id(), Context).

-spec get_destinations(_, _) -> _.
get_destinations(_Params, _Context) ->
    {ok, [#{
        <<"id">>         => <<"107498">>,
        <<"name">>       => <<"Squarey plastic thingy">>,
        <<"metadata">>   => #{<<"display_name">> => <<"Картофан СБЕР">>},
        <<"createdAt">>  => <<"2018-06-20T08:56:02Z">>,
        <<"isBlocked">>  => false,
        <<"identity">>   => <<"douknowdawae">>,
        <<"currency">>   => <<"RUB">>,
        <<"resource">>   => get_destination_resource(what, ever),
        <<"status">>     => <<"Authorized">>,
        <<"validUntil">> => <<"2018-06-20T08:56:02Z">>
    }]}.

-spec get_destination(_, _) -> _.
get_destination(_DestinationId, _Context) ->
    {ok, #{
        <<"id">>         => <<"107498">>,
        <<"name">>       => <<"Squarey plastic thingy">>,
        <<"metadata">>   => #{<<"display_name">> => <<"Картофан СБЕР">>},
        <<"createdAt">>  => <<"2018-06-20T08:56:02Z">>,
        <<"isBlocked">>  => false,
        <<"identity">>   => <<"douknowdawae">>,
        <<"currency">>   => <<"RUB">>,
        <<"resource">>   => get_destination_resource(what, ever),
        <<"status">>     => <<"Authorized">>,
        <<"validUntil">> => <<"2018-06-20T08:56:02Z">>
    }}.

-spec create_destination(_, _) -> _.
create_destination(_Params, Context) ->
    get_destination(woody_context:new_req_id(), Context).

-spec get_withdrawal(_, _) -> _.
get_withdrawal(WithdrawalId, _Context) ->
    {ok, #{
        <<"id">>          => WithdrawalId,
        <<"createdAt">>   => {{{2018, 06, 17}, {12, 01, 45}}, 0},
        <<"wallet">>      => woody_context:new_req_id(),
        <<"destination">> => woody_context:new_req_id(),
        <<"body">> => #{
            <<"amount">> => 1430000,
            <<"currency">> => <<"RUB">>
        },
        <<"status">>   => <<"Pending">>,
        <<"metadata">> => #{<<"who'sthedaddy">> => <<"me">>}
    }}.

-spec create_withdrawal(_, _) -> _.
create_withdrawal(_Params, Context) ->
    get_withdrawal(woody_context:new_req_id(), Context).

-spec get_withdrawal_events(_, _) -> _.
get_withdrawal_events(_, _) ->
    [#{
        <<"eventID">> => 1,
        <<"occuredAt">> => "2018-06-28T12:49:12Z",
        <<"changes">> => [#{
            <<"type">> => <<"WithdrawalStatusChanged">>,
            <<"status">> => <<"Pending">>
        }]
    },
    #{
        <<"eventID">> => 5,
        <<"occuredAt">> => "2018-06-28T12:49:13Z",
        <<"changes">> => [#{
            <<"type">> => <<"WithdrawalStatusChanged">>,
            <<"status">> => <<"Failed">>,
            <<"failure">> => <<"tolkonepiu is not a function">>
        }]
    }].

-spec get_withdrawal_event(_, _, _) -> _.
get_withdrawal_event(_WithdrawalId, EventId, _) ->
    #{
        <<"eventID">> => EventId,
        <<"occuredAt">> => "2018-07-24T04:37:45Z",
        <<"changes">> => [#{
            <<"type">> => <<"WithdrawalStatusChanged">>,
            <<"status">> => <<"Succeeded">>
        }]
    }.

%% Internals

get_destination_resource(_, _) ->
    #{
       <<"type">>          => <<"BankCardDestinationResource">>,
       <<"bin">>           => <<"424242">>,
       <<"lastDigits">>    => <<"4242">>,
       <<"paymentSystem">> => <<"visa">>,
       <<"token">>         => <<
           "eyJiaW4iOiI0MjQyNDIiLCJsYXN0RGlnaXRzIjoiNDI0MiIsInBheW1lbnRTeXN0ZW"
           "0iOiJ2aXNhIiwidG9rZW4iOiI3NXlQSkZac1lCOEFvdEFUS0dFa3p6In0"
       >>
    }.
