-module(p2p_fees).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-type wallet()        :: ff_wallet:wallet().
-type sender()        :: p2p_instrument:instrument().
-type receiver()      :: p2p_instrument:instrument().
-type cash()          :: ff_cash:cash().
-type terms()         :: dmsl_domain_thrift:'TermSet'().
-type plan_constant() :: operation_amount |
                        surplus.
-type token_payload() :: #{
    amount              := cash(),
    party_revision      := dmsl_domain_thrift:'PartyRevision'(),
    domain_revision     := ff_domain_config:revision(),
    cashflow            := ff_cash_flow:cash_flow_fee(),
    surplus_cash        => cash(),
    surplus_cash_volume => surplus_cash_volume()
}.
-type surplus_cash_volume() :: ff_cash_flow:plan_volume().

-opaque fees()  :: #{fees => #{plan_constant() => surplus_cash_volume()}}.
-opaque token() :: #{
    version := number(),
    payload := token_payload()
}.

-export_type([fees/0]).
-export_type([token/0]).

-export([surplus/1]).
-export([surplus_cash/1]).
-export([surplus_cash_volume/1]).
-export([get_fee_token/4]).

-import(ff_pipeline, [do/1, unwrap/2]).

%% Accessories

-spec surplus(fees()) ->
    surplus_cash_volume() | undefined.
surplus(#{fees := Fees}) ->
    maps:get(surplus, Fees, undefined).

-spec surplus_cash(token()) ->
    cash() | undefined.
surplus_cash(#{version := 1, payload := Payload}) ->
    maps:get(surplus_cash, Payload, undefined).

-spec surplus_cash_volume(token()) ->
    surplus_cash_volume() | undefined.
surplus_cash_volume(#{version := 1, payload := Payload}) ->
    maps:get(surplus_cash_volume, Payload, undefined).

%%
-spec create(token_payload()) ->
    token().
create(Payload) ->
    #{
        version => 1,
        payload => Payload
    }.

-spec get_fee_token(cash(), wallet(),  sender(), receiver()) ->
    {ok, p2p_fees:token()} |
    {error, {party, _Reason}} |
    {error, {cash_flow, _Reason}}.
get_fee_token({Amount, _} = Cash, Wallet, _Sender, _Receiver) ->
    do(fun() ->
        {ok, IdentityMachine} = ff_identity_machine:get(ff_wallet:identity(Wallet)),
        Identity = ff_identity_machine:identity(IdentityMachine),
        WalletID = ff_wallet:id(Wallet),
        PartyID = ff_identity:party(Identity),
        ContractID = ff_identity:contract(Identity),
        {ok, PartyRevision} = ff_party:get_revision(PartyID),
        DomainRevision = ff_domain_config:head(),
        VS = create_varset(PartyID, Cash),
        Terms = unwrap(party, ff_party:get_contract_terms(
            PartyID, ContractID, VS, ff_time:now(), PartyRevision, DomainRevision)),
        %% TODO I listened somethink about limit
        {ok, CashFlow} = p2p_party:get_cash_flow_plan(Terms),
        Fees = get_fees_from_terms(Terms),
        SurplusCashVolume = surplus(Fees),
        {ok, SurplusCash} = unwrap(cash_flow, compute_surplus_volume(SurplusCashVolume, Amount)),
        create(#{
            amount => Cash,
            wallet_id => WalletID,
            party_revision => PartyRevision,
            domain_revision => DomainRevision,
            cashflow => CashFlow,
            surplus_cash_volume => SurplusCashVolume,
            surplus_cash => SurplusCash
        })
    end).

compute_surplus_volume(undefined, _Amount) ->
    {ok, undefined};
compute_surplus_volume(CashVolume, Amount) ->
    Constants = #{operation_amount => Amount},
    p2p_party:compute_volume(CashVolume, Constants).

%%

create_varset(PartyID, {_, Currency} = Cash) ->
    #{
        party_id => PartyID,
        currency => encode_currency(Currency),
        cost     => encode_cash(Cash)
    }.

encode_cash({Amount, Currency}) ->
    #domain_Cash{
        amount   = Amount,
        currency = encode_currency(Currency)
    }.

encode_currency(Currency) ->
    #domain_CurrencyRef{symbolic_code = Currency}.

-spec get_fees_from_terms(terms()) ->
    fees().
get_fees_from_terms(Terms) ->
    #domain_TermSet{
        wallets = #domain_WalletServiceTerms{
            p2p = #domain_P2PServiceTerms{
                fees = FeeTerm
            }
        }
    } = Terms,
    decode_domain_fees(FeeTerm).

-spec decode_domain_fees(dmsl_domain_thrift:'Fees'() | undefined) ->
    fees().
decode_domain_fees(undefined) ->
    #{fees => #{}};
decode_domain_fees({value, #domain_Fees{fees = Fees}}) ->
    FeeDecode = maps:map(fun(_Key, Value) ->
        ff_cash_flow:decode_domain_plan_volume(Value)
    end, Fees),
    #{fees => FeeDecode}.
