%%%
%%% Copyright 2020 RBKmoney
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%

-module(ff_withdrawal_routing_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("shumpune_proto/include/shumpune_shumpune_thrift.hrl").

%% Common test API

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests

-export([adapter_unreachable_route_test/1]).
-export([adapter_unreachable_route_retryable_test/1]).
-export([adapter_unreachable_quote_test/1]).
-export([attempt_limit_test/1]).
-export([termial_priority_test/1]).

%% Internal types

-type config() :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name() :: ct_helper:group_name().
-type test_return() :: _ | no_return().

%% Macro helpers

-define(final_balance(Cash), {
    element(1, Cash),
    {
        {inclusive, element(1, Cash)},
        {inclusive, element(1, Cash)}
    },
    element(2, Cash)
}).

-define(final_balance(Amount, Currency), ?final_balance({Amount, Currency})).

%% Common test API implementation

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [
        {group, default}
    ].

-spec groups() -> [{group_name(), [test_case_name()]}].
groups() ->
    [
        {default, [
            adapter_unreachable_route_test,
            adapter_unreachable_route_retryable_test,
            adapter_unreachable_quote_test,
            attempt_limit_test,
            termial_priority_test
        ]}
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

%% Tests

-spec adapter_unreachable_route_test(config()) -> test_return().
adapter_unreachable_route_test(C) ->
    Currency = <<"RUB">>,
    Cash = {100500, Currency},
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, C),
    WithdrawalID = generate_id(),
    WithdrawalParams = #{
        id => WithdrawalID,
        destination_id => DestinationID,
        wallet_id => WalletID,
        body => Cash,
        external_id => WithdrawalID
    },
    ok = ff_withdrawal_machine:create(WithdrawalParams, ff_entity_context:new()),
    ?assertEqual(
        {failed, #{code => <<"authorization_error">>}},
        await_final_withdrawal_status(WithdrawalID)
    ).

-spec adapter_unreachable_route_retryable_test(config()) -> test_return().
adapter_unreachable_route_retryable_test(C) ->
    Currency = <<"RUB">>,
    Cash = {100500, Currency},
    #{
        wallet_id := WalletID,
        destination_id := DestinationID,
        party_id := PartyID
    } = prepare_standard_environment(Cash, C),
    _ = set_retryable_errors(PartyID, [<<"authorization_error">>]),
    WithdrawalID = generate_id(),
    WithdrawalParams = #{
        id => WithdrawalID,
        destination_id => DestinationID,
        wallet_id => WalletID,
        body => Cash,
        external_id => WithdrawalID
    },
    ok = ff_withdrawal_machine:create(WithdrawalParams, ff_entity_context:new()),
    ?assertEqual(succeeded, await_final_withdrawal_status(WithdrawalID)),
    ?assertEqual(?final_balance(0, Currency), get_wallet_balance(WalletID)),
    Withdrawal = get_withdrawal(WithdrawalID),
    ?assertEqual(WalletID, ff_withdrawal:wallet_id(Withdrawal)),
    ?assertEqual(DestinationID, ff_withdrawal:destination_id(Withdrawal)),
    ?assertEqual(Cash, ff_withdrawal:body(Withdrawal)),
    ?assertEqual(WithdrawalID, ff_withdrawal:external_id(Withdrawal)),
    _ = set_retryable_errors(PartyID, []).

-spec adapter_unreachable_quote_test(config()) -> test_return().
adapter_unreachable_quote_test(C) ->
    Currency = <<"RUB">>,
    Cash = {100500, Currency},
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, C),
    WithdrawalID = generate_id(),
    WithdrawalParams = #{
        id => WithdrawalID,
        destination_id => DestinationID,
        wallet_id => WalletID,
        body => Cash,
        external_id => WithdrawalID,
        quote => #{
            cash_from => Cash,
            cash_to => {2120, <<"USD">>},
            created_at => <<"2020-03-22T06:12:27Z">>,
            expires_on => <<"2020-03-22T06:12:27Z">>,
            route => ff_withdrawal_routing:make_route(4, 1),
            quote_data => #{<<"test">> => <<"test">>},
            operation_timestamp => ff_time:now()
        }
    },
    ok = ff_withdrawal_machine:create(WithdrawalParams, ff_entity_context:new()),
    ?assertEqual(
        {failed, #{code => <<"authorization_error">>}},
        await_final_withdrawal_status(WithdrawalID)
    ).

-spec attempt_limit_test(config()) -> test_return().
attempt_limit_test(C) ->
    Currency = <<"RUB">>,
    Cash = {500100, Currency},
    #{
        wallet_id := WalletID,
        destination_id := DestinationID
    } = prepare_standard_environment(Cash, C),
    WithdrawalID = generate_id(),
    WithdrawalParams = #{
        id => WithdrawalID,
        destination_id => DestinationID,
        wallet_id => WalletID,
        body => Cash,
        external_id => WithdrawalID
    },
    ok = ff_withdrawal_machine:create(WithdrawalParams, ff_entity_context:new()),
    ?assertEqual(
        {failed, #{code => <<"authorization_error">>}},
        await_final_withdrawal_status(WithdrawalID)
    ).

-spec termial_priority_test(config()) -> test_return().
termial_priority_test(C) ->
    Currency = <<"RUB">>,
    Cash = {500500, Currency},
    #{
        wallet_id := WalletID,
        destination_id := DestinationID,
        party_id := PartyID
    } = prepare_standard_environment(Cash, C),
    _ = set_retryable_errors(PartyID, [<<"authorization_error">>]),
    WithdrawalID = generate_id(),
    WithdrawalParams = #{
        id => WithdrawalID,
        destination_id => DestinationID,
        wallet_id => WalletID,
        body => Cash,
        external_id => WithdrawalID
    },
    ok = ff_withdrawal_machine:create(WithdrawalParams, ff_entity_context:new()),
    ?assertEqual(
        {failed, #{code => <<"not_expected_error">>}},
        await_final_withdrawal_status(WithdrawalID)
    ),
    _ = set_retryable_errors(PartyID, []).

%% Utils
set_retryable_errors(PartyID, ErrorList) ->
    application:set_env(ff_transfer, withdrawal, #{
        party_transient_errors => #{
            PartyID => ErrorList
        }
    }).

get_withdrawal(WithdrawalID) ->
    {ok, Machine} = ff_withdrawal_machine:get(WithdrawalID),
    ff_withdrawal_machine:withdrawal(Machine).

get_withdrawal_status(WithdrawalID) ->
    ff_withdrawal:status(get_withdrawal(WithdrawalID)).

await_final_withdrawal_status(WithdrawalID) ->
    finished = ct_helper:await(
        finished,
        fun() ->
            {ok, Machine} = ff_withdrawal_machine:get(WithdrawalID),
            Withdrawal = ff_withdrawal_machine:withdrawal(Machine),
            case ff_withdrawal:is_finished(Withdrawal) of
                false ->
                    {not_finished, Withdrawal};
                true ->
                    finished
            end
        end,
        genlib_retry:linear(10, 1000)
    ),
    get_withdrawal_status(WithdrawalID).

prepare_standard_environment({_Amount, Currency} = WithdrawalCash, C) ->
    Party = create_party(C),
    IdentityID = create_identity(Party, C),
    WalletID = create_wallet(IdentityID, <<"My wallet">>, Currency, C),
    ok = await_wallet_balance({0, Currency}, WalletID),
    DestinationID = create_destination(IdentityID, Currency, C),
    ok = set_wallet_balance(WithdrawalCash, WalletID),
    #{
        identity_id => IdentityID,
        party_id => Party,
        wallet_id => WalletID,
        destination_id => DestinationID
    }.

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

create_identity(Party, C) ->
    create_identity(Party, <<"good-one">>, C).

create_identity(Party, ProviderID, C) ->
    create_identity(Party, <<"Identity Name">>, ProviderID, C).

create_identity(Party, Name, ProviderID, _C) ->
    ID = genlib:unique(),
    ok = ff_identity_machine:create(
        #{id => ID, name => Name, party => Party, provider => ProviderID},
        #{<<"com.rbkmoney.wapi">> => #{<<"name">> => Name}}
    ),
    ID.

create_wallet(IdentityID, Name, Currency, _C) ->
    ID = genlib:unique(),
    ok = ff_wallet_machine:create(
        #{id => ID, identity => IdentityID, name => Name, currency => Currency},
        ff_entity_context:new()
    ),
    ID.

await_wallet_balance({Amount, Currency}, ID) ->
    Balance = {Amount, {{inclusive, Amount}, {inclusive, Amount}}, Currency},
    Balance = ct_helper:await(
        Balance,
        fun() -> get_wallet_balance(ID) end,
        genlib_retry:linear(3, 500)
    ),
    ok.

get_wallet_balance(ID) ->
    {ok, Machine} = ff_wallet_machine:get(ID),
    get_account_balance(ff_wallet:account(ff_wallet_machine:wallet(Machine))).

get_account_balance(Account) ->
    {ok, {Amounts, Currency}} = ff_transaction:balance(
        Account,
        ff_clock:latest_clock()
    ),
    {ff_indef:current(Amounts), ff_indef:to_range(Amounts), Currency}.

create_destination(IID, Currency, C) ->
    ID = generate_id(),
    {{Y, _, _}, _} = genlib_time:unixtime_to_daytime(erlang:system_time(second)),
    StoreSource = ct_cardstore:bank_card(<<"4150399999000900">>, {12, Y + 1}, C),
    Resource = {bank_card, #{bank_card => StoreSource}},
    Params = #{id => ID, identity => IID, name => <<"XDesination">>, currency => Currency, resource => Resource},
    ok = ff_destination_machine:create(Params, ff_entity_context:new()),
    authorized = ct_helper:await(
        authorized,
        fun() ->
            {ok, Machine} = ff_destination_machine:get(ID),
            Destination = ff_destination_machine:destination(Machine),
            ff_destination:status(Destination)
        end
    ),
    ID.

generate_id() ->
    ff_id:generate_snowflake_id().

set_wallet_balance({Amount, Currency}, ID) ->
    TransactionID = generate_id(),
    {ok, Machine} = ff_wallet_machine:get(ID),
    Account = ff_wallet:account(ff_wallet_machine:wallet(Machine)),
    AccounterID = ff_account:accounter_account_id(Account),
    {CurrentAmount, _, Currency} = get_account_balance(Account),
    {ok, AnotherAccounterID} = create_account(Currency),
    Postings = [{AnotherAccounterID, AccounterID, {Amount - CurrentAmount, Currency}}],
    {ok, _} = ff_transaction:prepare(TransactionID, Postings),
    {ok, _} = ff_transaction:commit(TransactionID, Postings),
    ok.

create_account(CurrencyCode) ->
    Description = <<"ff_test">>,
    case call_accounter('CreateAccount', {construct_account_prototype(CurrencyCode, Description)}) of
        {ok, Result} ->
            {ok, Result};
        {exception, Exception} ->
            {error, {exception, Exception}}
    end.

construct_account_prototype(CurrencyCode, Description) ->
    #shumpune_AccountPrototype{
        currency_sym_code = CurrencyCode,
        description = Description
    }.

call_accounter(Function, Args) ->
    Service = {shumpune_shumpune_thrift, 'Accounter'},
    ff_woody_client:call(accounter, {Service, Function, Args}, woody_context:new()).
