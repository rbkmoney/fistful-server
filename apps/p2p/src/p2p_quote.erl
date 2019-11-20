-module(p2p_quote).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-define(LIFETIME_MS_DEFAULT, 900000). %% 15min in milliseconds

-type sender()                    :: ff_resource:resource_params().
-type receiver()                  :: ff_resource:resource_params().
-type cash()                      :: ff_cash:cash().
-type terms()                     :: ff_party:terms().
-type plan_constant()             :: operation_amount | surplus.
-type identity()                  :: ff_identity:identity().
-type identity_id()               :: ff_identity:id().
-type compact_resource()          :: compact_bank_card_resource() | compact_crypto_wallet_resource().
-type surplus_cash_volume()       :: ff_cash_flow:plan_volume().
-type get_contract_terms_error()  :: ff_party:get_contract_terms_error().
-type validate_p2p_error()        :: ff_party:validate_p2p_error().
-type volume_finalize_error()     :: ff_cash_flow:volume_finalize_error().

-type compact_bank_card_resource() :: #{
    type := bank_card,
    token := binary(),
    bin_data_id => ff_bin_data:bin_data_id()
}.

-type compact_crypto_wallet_resource() :: #{
    type := crypto_wallet
}.

-type fees()        :: #{fees => #{plan_constant() => surplus_cash_volume()}}.
-opaque quote() :: #{
    amount            := cash(),
    party_revision    := ff_party:revision(),
    domain_revision   := ff_domain_config:revision(),
    created_at        := ff_time:timestamp_ms(),
    expires_on        := ff_time:timestamp_ms(),
    identity_id       := identity_id(),
    sender            := compact_resource(),
    receiver          := compact_resource()
}.

-export_type([quote/0]).
-export_type([get_contract_terms_error/0]).
-export_type([validate_p2p_error/0]).
-export_type([volume_finalize_error/0]).

-export([created_at/1]).
-export([domain_revision/1]).
-export([party_revision/1]).
-export([sender_id/1]).
-export([receiver_id/1]).
-export([get_quote/4]).
-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Accessories

-spec surplus(fees()) ->
    surplus_cash_volume() | undefined.
surplus(#{fees := Fees}) ->
    maps:get(surplus, Fees, undefined).

-spec created_at(quote()) ->
    ff_time:timestamp_ms().
created_at(#{created_at := Time}) ->
    Time.

-spec domain_revision(quote()) ->
    ff_domain_config:revision().
domain_revision(#{domain_revision := Revision}) ->
    Revision.

-spec party_revision(quote()) ->
    ff_party:revision().
party_revision(#{party_revision := Revision}) ->
    Revision.

-spec sender_id(quote()) ->
    ff_resource:resource_id() | undefined.
sender_id(#{sender := #{type := bank_card, bin_data_id := BinDataID}}) ->
    {bank_card, BinDataID};
sender_id(_) ->
    undefined.

-spec receiver_id(quote()) ->
    ff_resource:resource_id() | undefined.
receiver_id(#{receiver := #{type := bank_card, bin_data_id := BinDataID}}) ->
    {bank_card, BinDataID};
receiver_id(_) ->
    undefined.

-spec compact(ff_resource:resource()) ->
    compact_resource().
compact({bank_card, BankCard}) ->
    genlib_map:compact(#{
        type => bank_card,
        token => ff_resource:token(BankCard),
        bin_data_id => ff_resource:bin_data_id(BankCard)
    });
compact({crypto_wallet, _CryptoWallet}) ->
    #{type => crypto_wallet}.

%%

-spec get_quote(cash(), identity_id(), sender(), receiver()) ->
    {ok, {cash() | undefined, surplus_cash_volume() | undefined, quote()}} |
    {error, {identity,   not_found}} |
    {error, {party,      get_contract_terms_error()}} |
    {error, {cash_flow,  volume_finalize_error()}} |
    {error, {p2p_transfer:resource_owner(), {bin_data, not_found}}} |
    {error, {terms, validate_p2p_error()}}.
get_quote(Cash, IdentityID, Sender, Receiver) ->
    do(fun() ->
        SenderResource = unwrap(sender, ff_resource:create_resource(Sender)),
        ReceiverResource = unwrap(receiver, ff_resource:create_resource(Receiver)),
        Identity = unwrap(identity, get_identity(IdentityID)),
        {ok, PartyRevision} = ff_party:get_revision(ff_identity:party(Identity)),
        Params = #{
            cash => Cash,
            sender => SenderResource,
            receiver => ReceiverResource,
            party_revision => PartyRevision,
            domain_revision => ff_domain_config:head(),
            timestamp => ff_time:now()
        },
        {CreatedAt, PartyRevision, DomainRevision, Terms} =
            unwrap(p2p_party:get_contract_terms(Identity, Params)),
        valid = unwrap(terms, ff_party:validate_p2p(Terms, Cash)),

        ExpiresOn = get_expire_time(Terms, CreatedAt),
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
            sender => compact(SenderResource),
            receiver => compact(ReceiverResource)
        },
        {SurplusCash, SurplusCashVolume, Quote}
    end).

compute_surplus_volume(undefined, _Cash) ->
    {ok, undefined};
compute_surplus_volume(CashVolume, Cash) ->
    Constants = #{operation_amount => Cash},
    ff_cash_flow:compute_volume(CashVolume, Constants).

%%

-spec get_identity(identity_id()) ->
    {ok, identity()} | {error, notfound}.
get_identity(IdentityID) ->
    do(fun() ->
        IdentityMachine = unwrap(ff_identity_machine:get(IdentityID)),
        ff_identity_machine:identity(IdentityMachine)
    end).

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
