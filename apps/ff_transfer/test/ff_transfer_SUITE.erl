-module(ff_transfer_SUITE).

-include_lib("fistful_proto/include/ff_proto_fistful_admin_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_withdrawal_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").


-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([get_missing_fails/1]).
-export([deposit_via_admin_ok/1]).
-export([deposit_via_admin_fails/1]).
-export([deposit_via_admin_amount_fails/1]).
-export([deposit_via_admin_currency_fails/1]).
-export([deposit_withdrawal_ok/1]).
-export([deposit_quote_withdrawal_ok/1]).
-export([deposit_withdrawal_to_crypto_wallet/1]).

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
            get_missing_fails,
            deposit_via_admin_ok,
            deposit_via_admin_fails,
            deposit_via_admin_amount_fails,
            deposit_via_admin_currency_fails,
            deposit_withdrawal_ok,
            deposit_quote_withdrawal_ok,
            deposit_withdrawal_to_crypto_wallet
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

%%

-spec get_missing_fails(config()) -> test_return().
-spec deposit_via_admin_ok(config()) -> test_return().
-spec deposit_via_admin_fails(config()) -> test_return().
-spec deposit_via_admin_amount_fails(config()) -> test_return().
-spec deposit_via_admin_currency_fails(config()) -> test_return().
-spec deposit_withdrawal_ok(config()) -> test_return().
-spec deposit_withdrawal_to_crypto_wallet(config()) -> test_return().
-spec deposit_quote_withdrawal_ok(config()) -> test_return().

get_missing_fails(_C) ->
    ID = genlib:unique(),
    {error, {unknown_withdrawal, ID}} = ff_withdrawal_machine:get(ID).

deposit_via_admin_ok(C) ->
    Party = create_party(C),
    IID = create_person_identity(Party, C),
    WalID = create_wallet(IID, <<"HAHA NO">>, <<"RUB">>, C),
    ok = await_wallet_balance({0, <<"RUB">>}, WalID),
    SrcID = genlib:unique(),
    DepID = genlib:unique(),
    % Create source
    {ok, Src1} = call_admin('CreateSource', [#ff_admin_SourceParams{
        id       = SrcID,
        name     = <<"HAHA NO">>,
        identity_id = IID,
        currency = #'CurrencyRef'{symbolic_code = <<"RUB">>},
        resource = {internal, #src_Internal{details = <<"Infinite source of cash">>}}
    }]),

    SrcID = Src1#src_Source.id,
    {authorized, #src_Authorized{}} = ct_helper:await(
        {authorized, #src_Authorized{}},
        fun () ->
            {ok, Src} = call_admin('GetSource', [SrcID]),
            Src#src_Source.status
        end
    ),

    % Process deposit
    {ok, Dep1} = call_admin('CreateDeposit', [#ff_admin_DepositParams{
        id          = DepID,
        source      = SrcID,
        destination = WalID,
        body        = #'Cash'{
            amount   = 20000,
            currency = #'CurrencyRef'{symbolic_code = <<"RUB">>}
        }
    }]),
    DepID = Dep1#deposit_Deposit.id,
    {pending, _} = Dep1#deposit_Deposit.status,
    succeeded = ct_helper:await(
        succeeded,
        fun () ->
            {ok, Dep} = call_admin('GetDeposit', [DepID]),
            {Status, _} = Dep#deposit_Deposit.status,
            Status
        end,
        genlib_retry:linear(15, 1000)
    ),
    ok = await_wallet_balance({20000, <<"RUB">>}, WalID).

deposit_via_admin_fails(C) ->
    Party = create_party(C),
    IID = create_person_identity(Party, C),
    WalID = create_wallet(IID, <<"HAHA NO">>, <<"RUB">>, C),
    ok = await_wallet_balance({0, <<"RUB">>}, WalID),
    SrcID = genlib:unique(),
    DepID = genlib:unique(),
    % Create source
    {ok, Src1} = call_admin('CreateSource', [#ff_admin_SourceParams{
        id          = SrcID,
        name        = <<"HAHA NO">>,
        identity_id = IID,
        currency    = #'CurrencyRef'{symbolic_code = <<"RUB">>},
        resource    = {internal, #src_Internal{details = <<"Infinite source of cash">>}}
    }]),

    SrcID = Src1#src_Source.id,
    {authorized, #src_Authorized{}} = ct_helper:await(
        {authorized, #src_Authorized{}},
        fun () ->
            {ok, Src} = call_admin('GetSource', [SrcID]),
            Src#src_Source.status
        end
    ),

    {ok, Dep1} = call_admin('CreateDeposit', [
        #ff_admin_DepositParams{
            id          = DepID,
            source      = SrcID,
            destination = WalID,
            body        = #'Cash'{
                amount   = 10000002,
                currency = #'CurrencyRef'{symbolic_code = <<"RUB">>}
            }
        }
    ]),

    DepID = Dep1#deposit_Deposit.id,
    {pending, _} = Dep1#deposit_Deposit.status,
    failed = ct_helper:await(
        failed,
        fun () ->
            {ok, Dep} = call_admin('GetDeposit', [DepID]),
            {Status, _} = Dep#deposit_Deposit.status,
            Status
        end,
        genlib_retry:linear(15, 1000)
    ),
    ok = await_wallet_balance({0, <<"RUB">>}, WalID).

deposit_via_admin_amount_fails(C) ->
    Party = create_party(C),
    IID = create_person_identity(Party, C),
    WalID = create_wallet(IID, <<"HAHA NO">>, <<"RUB">>, C),
    ok = await_wallet_balance({0, <<"RUB">>}, WalID),
    SrcID = genlib:unique(),
    DepID = genlib:unique(),
    % Create source

    {ok, _Src1} = call_admin('CreateSource', [#ff_admin_SourceParams{
        id          = SrcID,
        name        = <<"HAHA NO">>,
        identity_id = IID,
        currency    = #'CurrencyRef'{symbolic_code = <<"RUB">>},
        resource    = {internal, #src_Internal{details = <<"Infinite source of cash">>}}
    }]),

    {authorized, #src_Authorized{}} = ct_helper:await(
        {authorized, #src_Authorized{}},
        fun () ->
            {ok, Src} = call_admin('GetSource', [SrcID]),
            Src#src_Source.status
        end
    ),

    {exception, #ff_admin_DepositAmountInvalid{}} = call_admin('CreateDeposit', [
        #ff_admin_DepositParams{
            id          = DepID,
            source      = SrcID,
            destination = WalID,
            body        = #'Cash'{
                amount   = -1,
                currency = #'CurrencyRef'{symbolic_code = <<"RUB">>}
            }
        }
    ]),
    ok = await_wallet_balance({0, <<"RUB">>}, WalID).

deposit_via_admin_currency_fails(C) ->
    Party = create_party(C),
    IID = create_person_identity(Party, C),
    WalID = create_wallet(IID, <<"HAHA NO">>, <<"RUB">>, C),
    ok = await_wallet_balance({0, <<"RUB">>}, WalID),
    SrcID = genlib:unique(),
    DepID = genlib:unique(),
    % Create source
    {ok, Src1} = call_admin('CreateSource', [#ff_admin_SourceParams{
        id          = SrcID,
        name        = <<"HAHA NO">>,
        identity_id = IID,
        currency    = #'CurrencyRef'{symbolic_code = <<"RUB">>},
        resource    = {internal, #src_Internal{details = <<"Infinite source of cash">>}}
    }]),

    SrcID = Src1#src_Source.id,
    {authorized, #src_Authorized{}} = ct_helper:await(
        {authorized, #src_Authorized{}},
        fun () ->
            {ok, Src} = call_admin('GetSource', [SrcID]),
            Src#src_Source.status
        end
    ),
    BadCurrency = <<"CAT">>,
    {exception, #ff_admin_DepositCurrencyInvalid{}} = call_admin('CreateDeposit', [#ff_admin_DepositParams{
            id          = DepID,
            source      = SrcID,
            destination = WalID,
            body        = #'Cash'{
                amount   = 1000,
                currency = #'CurrencyRef'{symbolic_code = BadCurrency}
            }
        }]),

    ok = await_wallet_balance({0, <<"RUB">>}, WalID).

deposit_withdrawal_ok(C) ->
    Party = create_party(C),
    IID = create_person_identity(Party, C),
    ICID = genlib:unique(),
    WalID = create_wallet(IID, <<"HAHA NO">>, <<"RUB">>, C),
    ok = await_wallet_balance({0, <<"RUB">>}, WalID),

    SrcID = create_source(IID, C),

    process_deposit(SrcID, WalID),

    DestID = create_destination(IID, C),

    pass_identification(ICID, IID, C),

    WdrID     = process_withdrawal(WalID, DestID),
    Events    = get_withdrawal_events(WdrID),
    [<<"1">>] = route_changes(Events).

deposit_withdrawal_to_crypto_wallet(C) ->
    Party  = create_party(C),
    IID    = create_person_identity(Party, C),
    ICID   = genlib:unique(),
    WalID  = create_wallet(IID, <<"WalletName">>, <<"RUB">>, C),
    ok     = await_wallet_balance({0, <<"RUB">>}, WalID),
    SrcID  = create_source(IID, C),
    ok     = process_deposit(SrcID, WalID),
    DestID = create_crypto_destination(IID, C),
    pass_identification(ICID, IID, C),
    WdrID     = process_withdrawal(WalID, DestID),
    Events    = get_withdrawal_events(WdrID),
    [<<"2">>] = route_changes(Events).

deposit_quote_withdrawal_ok(C) ->
    Party  = create_party(C),
    IID = create_person_identity(Party, C, <<"quote-owner">>),
    ICID = genlib:unique(),
    WalID = create_wallet(IID, <<"HAHA NO">>, <<"RUB">>, C),
    ok = await_wallet_balance({0, <<"RUB">>}, WalID),

    SrcID = create_source(IID, C),

    process_deposit(SrcID, WalID),

    DestID = create_destination(IID, C),

    pass_identification(ICID, IID, C),

    WdrID = process_withdrawal(WalID, DestID, #{
        wallet_id => WalID,
        destination_id => DestID,
        body => {4240, <<"RUB">>},
        quote => #{
            cash_from   => {4240, <<"RUB">>},
            cash_to     => {2120, <<"USD">>},
            created_at  => <<"2016-03-22T06:12:27Z">>,
            expires_on  => <<"2016-03-22T06:12:27Z">>,
            quote_data  => #{
                <<"version">> => 1,
                <<"quote_data">> => #{<<"test">> => <<"test">>},
                <<"provider_id">> => 3,
                <<"terminal_id">> => 1
            }
        }
    }),

    Events    = get_withdrawal_events(WdrID),
    [<<"3">>] = route_changes(Events).

create_party(_C) ->
    ID = genlib:bsuuid(),
    _ = ff_party:create(ID),
    ID.

create_person_identity(Party, C) ->
    create_person_identity(Party, C, <<"good-one">>).

create_person_identity(Party, C, ProviderID) ->
    create_identity(Party, ProviderID, <<"person">>, C).

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

await_wallet_balance({Amount, Currency}, ID) ->
    Balance = {Amount, {{inclusive, Amount}, {inclusive, Amount}}, Currency},
    Balance = ct_helper:await(
        Balance,
        fun () -> get_wallet_balance(ID) end,
        genlib_retry:linear(3, 500)
    ),
    ok.

await_destination_balance({Amount, Currency}, ID) ->
    Balance = {Amount, {{inclusive, Amount}, {inclusive, Amount}}, Currency},
    Balance = ct_helper:await(
        Balance,
        fun () -> get_destination_balance(ID) end,
        genlib_retry:linear(3, 500)
    ),
    ok.

get_wallet_balance(ID) ->
    {ok, Machine} = ff_wallet_machine:get(ID),
    get_account_balance(ff_wallet:account(ff_wallet_machine:wallet(Machine))).

get_destination_balance(ID) ->
    {ok, Machine} = ff_destination:get_machine(ID),
    get_account_balance(ff_destination:account(ff_destination:get(Machine))).

get_account_balance(Account) ->
    {ok, {Amounts, Currency}} = ff_transaction:balance(
        Account,
        ff_clock:latest_clock()
    ),
    {ff_indef:current(Amounts), ff_indef:to_range(Amounts), Currency}.

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

call_admin(Fun, Args) ->
    Service = {ff_proto_fistful_admin_thrift, 'FistfulAdmin'},
    Request = {Service, Fun, Args},
    Client  = ff_woody_client:new(#{
        url           => <<"http://localhost:8022/v1/admin">>,
        event_handler => scoper_woody_event_handler
    }),
    ff_woody_client:call(Client, Request).

create_source(IID, C) ->
    % Create source
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
    succeeded = ct_helper:await(
        succeeded,
        fun () ->
            {ok, DepM} = ff_deposit_machine:get(DepID),
            ff_deposit:status(ff_deposit_machine:deposit(DepM))
        end,
        genlib_retry:linear(15, 1000)
    ),
    ok = await_wallet_balance({10000, <<"RUB">>}, WalID).

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

create_crypto_destination(IID, C) ->
    Resource = {crypto_wallet, #{crypto_wallet => #{
        id => <<"a30e277c07400c9940628828949efd48">>,
        currency => {litecoin, #{}}
    }}},
    DestID = create_instrument(destination, IID, <<"CryptoDestination">>, <<"RUB">>, Resource, C),
    authorized = ct_helper:await(
        authorized,
        fun () ->
            {ok, DestM} = ff_destination:get_machine(DestID),
            ff_destination:status(ff_destination:get(DestM))
        end
    ),
    DestID.

pass_identification(ICID, IID, C) ->
    Doc1 = ct_identdocstore:rus_retiree_insurance_cert(genlib:unique(), C),
    Doc2 = ct_identdocstore:rus_domestic_passport(C),
    ok = ff_identity_machine:start_challenge(
        IID, #{
            id     => ICID,
            class  => <<"sword-initiation">>,
            proofs => [Doc1, Doc2]
        }
    ),
    {completed, _} = ct_helper:await(
        {completed, #{resolution => approved}},
        fun () ->
            {ok, S}  = ff_identity_machine:get(IID),
            {ok, IC} = ff_identity:challenge(ICID, ff_identity_machine:identity(S)),
            ff_identity_challenge:status(IC)
        end
    ).

process_withdrawal(WalID, DestID) ->
    process_withdrawal(WalID, DestID, #{wallet_id => WalID, destination_id => DestID, body => {4240, <<"RUB">>}}).
process_withdrawal(WalID, DestID, Params) ->
    WdrID = generate_id(),
    ok = ff_withdrawal_machine:create(
        Params#{id => WdrID},
        ff_entity_context:new()
    ),
    succeeded = ct_helper:await(
        succeeded,
        fun () ->
            {ok, WdrM} = ff_withdrawal_machine:get(WdrID),
            ff_withdrawal:status(ff_withdrawal_machine:withdrawal(WdrM))
        end,
        genlib_retry:linear(15, 1000)
    ),
    ok = await_wallet_balance({10000 - 4240, <<"RUB">>}, WalID),
    ok = await_destination_balance({4240 - 848, <<"RUB">>}, DestID),
    WdrID.

%%%

get_withdrawal_events(WdrID) ->
    Service = {{ff_proto_withdrawal_thrift, 'Management'}, <<"/v1/withdrawal">>},
    {ok, Events} = call('GetEvents', Service, [WdrID, #'EventRange'{'after' = 0, limit = 1000}]),
    Events.

call(Function, Service, Args) ->
    call(Function, Service, Args, <<"8022">>).

call(Function, {Service, Path}, Args, Port) ->
    Request = {Service, Function, Args},
    Client  = ff_woody_client:new(#{
        url           => <<"http://localhost:", Port/binary, Path/binary>>,
        event_handler => scoper_woody_event_handler
    }),
    ff_woody_client:call(Client, Request).

route_changes(Events) ->
    lists:filtermap(
        fun
            (#wthd_Event{change = {route, RouteChange}}) ->
                #wthd_RouteChange{route = #wthd_Route{provider_id = ProviderID}} = RouteChange,
                {true, ProviderID};
            (_Other) ->
                false
        end,
        Events
    ).
