-module(p2p_party).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-type identity_id()     :: ff_identity:id().
-type terms()           :: ff_party:terms().
-type contract_params() :: #{
    cash            := ff_cash:cash(),
    sender          := ff_resource:resource(),
    receiver        := ff_resource:resource(),
    party_revision  => ff_party:revision(),
    domain_revision => ff_domain_config:revision(),
    timestamp       => ff_time:timestamp_ms()
}.
-type varset()        :: hg_selector:varset().
-type varset_params() :: #{
    cash        := ff_cash:cash(),
    party_id    := ff_party:id(),
    sender      := ff_resource:resource(),
    receiver    := ff_resource:resource()
}.

-export_type([varset/0]).
-export([create_varset/1]).
-export([get_contract_terms/2]).
-export([allow_p2p_tool/1]).

-import(ff_pipeline, [do/1, unwrap/2]).

-spec create_varset(varset_params()) ->
    varset().
create_varset(#{cash := Cash, sender := Sender, receiver := Receiver} = Params) ->
    {_, Currency} = Cash,
    #{
        party_id => maps:get(party_id, Params),
        currency => ff_dmsl_codec:marshal(currency_ref, Currency),
        cost     => ff_dmsl_codec:marshal(cash, Cash),
        p2p_tool => ff_dmsl_codec:marshal(p2p_tool, {Sender, Receiver})
    }.

-spec get_contract_terms(identity_id(), contract_params()) ->
    {ok, {ff_time:timestamp_ms(), ff_party:revision(), ff_domain_config:revision(), terms()}} |
    {error, {identity, not_found}} |
    {error, {party, ff_party:get_contract_terms_error()}}.
get_contract_terms(IdentityID, Params) ->
    do(fun() ->
        IdentityMachine = unwrap(identity, ff_identity_machine:get(IdentityID)),
        Identity = ff_identity_machine:identity(IdentityMachine),
        PartyID = ff_identity:party(Identity),
        ContractID = ff_identity:contract(Identity),
        PartyRev = maps:get(party_revision, Params, undefined),

        PartyRevision = get_party_revision(PartyRev, PartyID),
        DomainRevision = maps:get(domain_revision, Params, ff_domain_config:head()),
        Timestamp = maps:get(timestamp, Params, ff_time:now()),

        VS = create_varset(Params#{party_id => PartyID}),

        Terms = unwrap(party, ff_party:get_contract_terms(
            PartyID, ContractID, VS, Timestamp, PartyRevision, DomainRevision)),
        {Timestamp, PartyRevision, DomainRevision, Terms}
    end).

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

get_party_revision(undefined, PartyID) ->
    {ok, PartyRevision} = ff_party:get_revision(PartyID),
    PartyRevision;
get_party_revision(PartyRev, _) ->
    PartyRev.
