-module(p2p_fees).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-define(LIFETIME_MS_DEFAULT, 900000). %% 15min in milliseconds

-type sender()        :: p2p_instrument:instrument().
-type receiver()      :: p2p_instrument:instrument().
-type cash()          :: ff_cash:cash().
-type terms()         :: ff_party:terms().
-type plan_constant() :: operation_amount | surplus.
-type identity_id()   :: ff_identity:id().
-type instrument()    :: #{token := binary(), bin_data_id => ff_bin_data:bin_data_id()}.
-type surplus_cash_volume()       :: ff_cash_flow:plan_volume().
-type get_contract_terms_error()  :: ff_party:get_contract_terms_error().
-type validate_p2p_error()        :: ff_party:validate_p2p_error().
-type volume_finalize_error()     :: ff_cash_flow:volume_finalize_error().

-type fees()        :: #{fees => #{plan_constant() => surplus_cash_volume()}}.
-opaque fee_quote() :: #{
    amount            := cash(),
    party_revision    := ff_party:revision(),
    domain_revision   := ff_domain_config:revision(),
    created_at        := ff_time:timestamp_ms(),
    expires_on        := ff_time:timestamp_ms(),
    identity_id       := identity_id(),
    sender            := instrument(),
    receiver          := instrument()
}.

-export_type([fee_quote/0]).
-export_type([get_contract_terms_error/0]).
-export_type([validate_p2p_error/0]).
-export_type([volume_finalize_error/0]).

-export([created_at/1]).
-export([domain_revision/1]).
-export([party_revision/1]).
-export([sender_id/1]).
-export([receiver_id/1]).
-export([get_fee_quote/4]).
-import(ff_pipeline, [do/1, unwrap/2]).

%% Accessories

-spec surplus(fees()) ->
    surplus_cash_volume() | undefined.
surplus(#{fees := Fees}) ->
    maps:get(surplus, Fees, undefined).

-spec created_at(fee_quote()) ->
    ff_time:timestamp_ms().
created_at(#{created_at := Time}) ->
    Time.

-spec domain_revision(fee_quote()) ->
    ff_domain_config:revision().
domain_revision(#{domain_revision := Revision}) ->
    Revision.

-spec party_revision(fee_quote()) ->
    ff_party:revision().
party_revision(#{party_revision := Revision}) ->
    Revision.


-spec sender_id(fee_quote()) ->
    ff_bin_data:bin_data_id().
sender_id(#{sender := Sender}) ->
    maps:get(bin_data_id, Sender).

-spec receiver_id(fee_quote()) ->
    ff_bin_data:bin_data_id().
receiver_id(#{receiver := Receiver}) ->
    maps:get(bin_data_id, Receiver).

-spec instrument(p2p_instrument:instrument()) ->
    instrument().
instrument(Instrument) ->
    genlib_map:compact(#{
        token => p2p_instrument:token(Instrument),
        bin_data_id => p2p_instrument:bin_data_id(Instrument)
    }).

%%

-spec get_fee_quote(cash(), identity_id(), sender(), receiver()) ->
    {ok, {cash() | undefined, surplus_cash_volume() | undefined, fee_quote()}} |
    {error, {identity,   not_found}} |
    {error, {party,      get_contract_terms_error()}} |
    {error, {p2p_tool,   not_allow}} |
    {error, {cash_flow,  volume_finalize_error()}} |
    {error, {terms, validate_p2p_error()}}.
get_fee_quote(Cash, IdentityID, Sender, Receiver) ->
    do(fun() ->
        IdentityMachine = unwrap(identity, ff_identity_machine:get(IdentityID)),
        Identity = ff_identity_machine:identity(IdentityMachine),
        PartyID = ff_identity:party(Identity),
        ContractID = ff_identity:contract(Identity),
        {ok, PartyRevision} = ff_party:get_revision(PartyID),
        DomainRevision = ff_domain_config:head(),
        Params = #{
            cash => Cash,
            party_id => PartyID,
            sender => Sender,
            receiver => Receiver
        },
        VS = p2p_party:create_varset(Params),
        CreatedAt = ff_time:now(),
        Terms = unwrap(party, ff_party:get_contract_terms(
            PartyID, ContractID, VS, CreatedAt, PartyRevision, DomainRevision)),
        valid = unwrap(terms, ff_party:validate_p2p(Terms, Cash)),
        ExpiresOn = get_expire_time(Terms, CreatedAt),
        true = unwrap(p2p_tool, allow_p2p_tool(Terms)),
        Fees = get_fees_from_terms(Terms),
        SurplusCashVolume = surplus(Fees),
        SurplusCash = unwrap(cash_flow, compute_surplus_volume(SurplusCashVolume, Cash)),
        Quote = #{
            amount => Cash,
            party_revision => PartyRevision,
            domain_revision => DomainRevision,
            created_at => CreatedAt,
            expires_on => ExpiresOn,
            identity_id => IdentityID,
            sender => instrument(Sender),
            receiver => instrument(Receiver)
        },
        {SurplusCash, SurplusCashVolume, Quote}
    end).

compute_surplus_volume(undefined, _Cash) ->
    {ok, undefined};
compute_surplus_volume(CashVolume, Cash) ->
    Constants = #{operation_amount => Cash},
    ff_cash_flow:compute_volume(CashVolume, Constants).

%%

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

-spec allow_p2p_tool(terms()) ->
    {ok, true} | {error, not_allow}.
allow_p2p_tool(Terms) ->
    #domain_TermSet{wallets = WalletTerms} = Terms,
    #domain_WalletServiceTerms{p2p = P2PServiceTerms} = WalletTerms,
    #domain_P2PServiceTerms{allow = Constant} = P2PServiceTerms,
    case Constant of
        {constant, true} -> {ok, true};
        {constant, false} -> {error, not_allow}
    end.

-spec get_expire_time(terms(), ff_time:timestamp_ms()) ->
    ff_time:timestamp_ms().
get_expire_time(Terms, CreatedAt) ->
    #domain_TermSet{wallets = WalletTerms} = Terms,
    #domain_WalletServiceTerms{p2p = P2PServiceTerms} = WalletTerms,
    #domain_P2PServiceTerms{quote_lifetime = Lifetime} = P2PServiceTerms,
    case Lifetime of
        undefined ->
            CreatedAt + ?LIFETIME_MS_DEFAULT;
        {value, {interval, LifetimeInterval}} ->
            DateTime = decode_lifetime_interval(LifetimeInterval),
            ff_time:add_interval(CreatedAt, DateTime)
    end.

decode_lifetime_interval(LifetimeInterval) ->
    #domain_LifetimeInterval{
        years = YY,
        months = MM,
        days = DD,
        hours = HH,
        minutes = Min,
        seconds = Sec
    } = LifetimeInterval,
    Date = {nvl(YY), nvl(MM), nvl(DD)},
    Time = {nvl(HH), nvl(Min), nvl(Sec)},
    {Date, Time}.

nvl(Val) ->
    nvl(Val, 0).

nvl(undefined, Default) ->
    Default;
nvl(Val, _) ->
    Val.
