-module(p2p_fees).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-type sender()        :: p2p_instrument:instrument().
-type receiver()      :: p2p_instrument:instrument().
-type cash()          :: ff_cash:cash().
-type terms()         :: ff_party:terms().
-type plan_constant() :: operation_amount | surplus.
-type identity_id()   :: ff_identity:id().
-type instrument()    :: #{token := binary(), bin_data_id => ff_bin_data:bin_data_id()}.
-type token_payload() :: #{
    amount            := cash(),
    party_revision    := ff_party:revision(),
    domain_revision   := ff_party:domain_revision(),
    created_at        := ff_time:timestamp_ms(),
    identity_id       := identity_id(),
    sender            := instrument(),
    receiver          := instrument()
}.
-type surplus_cash_volume()       :: ff_cash_flow:plan_volume().
-type get_contract_terms_error()  :: ff_party:get_contract_terms_error().
-type validate_p2p_error()        :: ff_party:validate_p2p_error().
-type volume_finalize_error()     :: ff_cash_flow:volume_finalize_error().

-type fees()    :: #{fees => #{plan_constant() => surplus_cash_volume()}}.
-opaque token() :: #{
    version := number(),
    payload := token_payload()
}.

-export_type([token/0]).
-export_type([get_contract_terms_error/0]).
-export_type([validate_p2p_error/0]).
-export_type([volume_finalize_error/0]).

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

-spec instrument(p2p_instrument:instrument()) ->
    instrument().
instrument(Instrument) ->
    genlib_map:compact(#{
        token => p2p_instrument:token(Instrument),
        bin_data_id => p2p_instrument:bin_data_id(Instrument)
    }).

%%
-spec create(token_payload()) ->
    token().
create(Payload) ->
    #{
        version => 1,
        payload => Payload
    }.

-spec get_fee_token(cash(), identity_id(), sender(), receiver()) ->
    {ok, {cash(), p2p_fees:token()}} |
    {error, {party,      get_contract_terms_error()}} |
    {error, {validation, validate_p2p_error()}}       |
    {error, {cash_flow,  volume_finalize_error()}}.
get_fee_token({Amount, _} = Cash, IdentityID, Sender, Receiver) ->
    do(fun() ->
        {ok, IdentityMachine} = ff_identity_machine:get(IdentityID),
        Identity = ff_identity_machine:identity(IdentityMachine),
        PartyID = ff_identity:party(Identity),
        ContractID = ff_identity:contract(Identity),
        {ok, PartyRevision} = ff_party:get_revision(PartyID),
        DomainRevision = ff_domain_config:head(),
        VS = create_varset(PartyID, Cash, Sender, Receiver),
        Timestamp = ff_time:now(),
        Terms = unwrap(party, ff_party:get_contract_terms(
            PartyID, ContractID, VS, Timestamp, PartyRevision, DomainRevision)),
        valid = unwrap(validation, ff_party:validate_p2p_limits(Terms, Cash)),
        Fees = get_fees_from_terms(Terms),
        SurplusCashVolume = surplus(Fees),
        SurplusCash = unwrap(cash_flow, compute_surplus_volume(SurplusCashVolume, Amount)),
        Token = create(genlib_map:compact(#{
            amount => Cash,
            party_revision => PartyRevision,
            domain_revision => DomainRevision,
            created_at => Timestamp,
            identity_id => IdentityID,
            sender => instrument(Sender),
            receiver => instrument(Receiver)
        })),
        {SurplusCash, Token}
    end).

compute_surplus_volume(undefined, _Amount) ->
    {ok, undefined};
compute_surplus_volume(CashVolume, Amount) ->
    Constants = #{operation_amount => Amount},
    ff_cash_flow:compute_volume(CashVolume, Constants).

%%

create_varset(PartyID, {_, Currency} = Cash, Sender, Receiver) ->
    #{
        party_id => PartyID,
        currency => encode_currency(Currency),
        cost     => encode_cash(Cash),
        p2p_tool => encode_p2p_tool(Sender, Receiver)
    }.

encode_cash({Amount, Currency}) ->
    #domain_Cash{
        amount   = Amount,
        currency = encode_currency(Currency)
    }.

encode_currency(Currency) ->
    #domain_CurrencyRef{symbolic_code = Currency}.

encode_p2p_tool(Sender, Receiver) ->
    #domain_P2PTool{
        sender = p2p_instrument:construct_payment_tool(Sender),
        receiver = p2p_instrument:construct_payment_tool(Receiver)
    }.

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
