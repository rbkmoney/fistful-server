-module(p2p_quote).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-define(LIFETIME_MS_DEFAULT, 900000). %% 15min in milliseconds

-type sender()                    :: ff_resource:resource_params().
-type receiver()                  :: ff_resource:resource_params().
-type cash()                      :: ff_cash:cash().
-type terms()                     :: ff_party:terms().
-type identity()                  :: ff_identity:identity_state().
-type identity_id()               :: ff_identity:id().
-type compact_resource()          :: compact_bank_card_resource().
-type get_contract_terms_error()  :: ff_party:get_contract_terms_error().
-type validate_p2p_error()        :: ff_party:validate_p2p_error().
-type volume_finalize_error()     :: ff_cash_flow:volume_finalize_error().

-type get_quote_error() ::
    {identity, not_found} |
    {party, get_contract_terms_error()} |
    {fees, ff_fees_plan:computation_error()} |
    {p2p_transfer:resource_owner(), {bin_data, ff_bin_data:bin_data_error()}} |
    {terms, validate_p2p_error()}.

-type compact_bank_card_resource() :: {bank_card, #{
    token := binary(),
    bin_data_id := ff_bin_data:bin_data_id()
}}.

-type quote() :: #{
    fees              := ff_fees_final:fees(),
    amount            := cash(),
    party_revision    := ff_party:revision(),
    domain_revision   := ff_domain_config:revision(),
    created_at        := ff_time:timestamp_ms(),
    expires_on        := ff_time:timestamp_ms(),
    identity_id       := identity_id(),
    sender            := compact_resource(),
    receiver          := compact_resource()
}.

-type params() :: #{
    body := cash(),
    identity_id := identity_id(),
    sender := sender(),
    receiver := receiver()
}.

-export_type([quote/0]).
-export_type([params/0]).
-export_type([get_contract_terms_error/0]).
-export_type([validate_p2p_error/0]).
-export_type([volume_finalize_error/0]).
-export_type([get_quote_error/0]).

%% Accessors

-export([fees/1]).
-export([amount/1]).
-export([created_at/1]).
-export([expires_on/1]).
-export([domain_revision/1]).
-export([party_revision/1]).
-export([identity_id/1]).
-export([sender/1]).
-export([receiver/1]).
-export([sender_descriptor/1]).
-export([receiver_descriptor/1]).

%% API

-export([get/1]).
-export([get_quote/4]).
-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Accessors

-spec fees(quote()) ->
    ff_fees_final:fees().
fees(#{fees := Fees}) ->
    Fees.

-spec amount(quote()) ->
    cash().
amount(#{amount := Amount}) ->
    Amount.

-spec created_at(quote()) ->
    ff_time:timestamp_ms().
created_at(#{created_at := Time}) ->
    Time.

-spec expires_on(quote()) ->
    ff_time:timestamp_ms().
expires_on(#{expires_on := Time}) ->
    Time.

-spec domain_revision(quote()) ->
    ff_domain_config:revision().
domain_revision(#{domain_revision := Revision}) ->
    Revision.

-spec party_revision(quote()) ->
    ff_party:revision().
party_revision(#{party_revision := Revision}) ->
    Revision.

-spec identity_id(quote()) ->
    identity_id().
identity_id(#{identity_id := IdentityID}) ->
    IdentityID.

-spec sender(quote()) ->
    compact_resource().
sender(#{sender := Sender}) ->
    Sender.

-spec receiver(quote()) ->
    compact_resource().
receiver(#{receiver := Receiver}) ->
    Receiver.

-spec sender_descriptor(quote()) ->
    ff_resource:resource_descriptor().
sender_descriptor(#{sender := {bank_card, #{bin_data_id := BinDataID}}}) ->
    {bank_card, BinDataID}.

-spec receiver_descriptor(quote()) ->
    ff_resource:resource_descriptor().
receiver_descriptor(#{receiver := {bank_card, #{bin_data_id := BinDataID}}}) ->
    {bank_card, BinDataID}.

-spec compact(ff_resource:resource()) ->
    compact_resource().
compact({bank_card, #{bank_card := BankCard}}) ->
    {bank_card, #{
        token => ff_resource:token(BankCard),
        bin_data_id => ff_resource:bin_data_id(BankCard)
    }}.

%%

-spec get(params()) ->
    {ok, quote()} |
    {error, get_quote_error()}.
get(#{
    body := Cash,
    identity_id := IdentityID,
    sender := Sender,
    receiver := Receiver
}) ->
    get_quote(Cash, IdentityID, Sender, Receiver).

-spec get_quote(cash(), identity_id(), sender(), receiver()) ->
    {ok, quote()} |
    {error, get_quote_error()}.
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
        ComputedFees = unwrap(fees, ff_fees_plan:compute(Fees, Cash)),
        genlib_map:compact(#{
            fees => ComputedFees,
            amount => Cash,
            party_revision => PartyRevision,
            domain_revision => DomainRevision,
            created_at => CreatedAt,
            expires_on => ExpiresOn,
            identity_id => IdentityID,
            sender => compact(SenderResource),
            receiver => compact(ReceiverResource)
        })
    end).

%%

-spec get_identity(identity_id()) ->
    {ok, identity()} | {error, notfound}.
get_identity(IdentityID) ->
    do(fun() ->
        IdentityMachine = unwrap(ff_identity_machine:get(IdentityID)),
        ff_identity_machine:identity(IdentityMachine)
    end).

-spec get_fees_from_terms(terms()) ->
    ff_fees_plan:fees().
get_fees_from_terms(Terms) ->
    #domain_TermSet{
        wallets = #domain_WalletServiceTerms{
            p2p = #domain_P2PServiceTerms{
                fees = FeeTerm
            }
        }
    } = Terms,
    decode_domain_fees(FeeTerm).

-spec decode_domain_fees(dmsl_domain_thrift:'FeeSelector'() | undefined) ->
    ff_fees_plan:fees().
decode_domain_fees(undefined) ->
    #{fees => #{}};
decode_domain_fees({value, Fees}) -> % must be reduced before
    ff_fees_plan:unmarshal(Fees).

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
