-module(p2p_party).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-type identity()        :: ff_identity:identity().
-type terms()           :: ff_party:terms().
-type contract_params() :: #{
    cash            := ff_cash:cash(),
    sender          := ff_resource:resource(),
    receiver        := ff_resource:resource(),
    party_revision  := ff_party:revision(),
    domain_revision := ff_domain_config:revision(),
    timestamp       := ff_time:timestamp_ms()
}.
-type varset()        :: hg_selector:varset().
-type varset_params() :: #{
    cash        := ff_cash:cash(),
    party_id    := ff_party:id(),
    sender      := ff_resource:resource(),
    receiver    := ff_resource:resource(),
    risk_score  => p2p_inspector:risk_score()
}.

-export_type([varset/0]).
-export_type([contract_params/0]).
-export([create_varset/1]).
-export([get_contract_terms/2]).

-import(ff_pipeline, [do/1, unwrap/2]).

-spec create_varset(varset_params()) ->
    varset().
create_varset(#{cash := Cash, sender := Sender, receiver := Receiver} = Params) ->
    {_, Currency} = Cash,
    genlib_map:compact(#{
        party_id => maps:get(party_id, Params),
        currency => ff_dmsl_codec:marshal(currency_ref, Currency),
        cost     => ff_dmsl_codec:marshal(cash, Cash),
        p2p_tool => ff_dmsl_codec:marshal(p2p_tool, {Sender, Receiver}),
        risk_score => ff_dmsl_codec:marshal(risk_score, maps:get(risk_score, Params, undefined))
    }).

-spec get_contract_terms(identity(), contract_params()) ->
    {ok, {ff_time:timestamp_ms(), ff_party:revision(), ff_domain_config:revision(), terms()}} |
    {error, {party, ff_party:get_contract_terms_error()}}.
get_contract_terms(Identity, Params) ->
    do(fun() ->
        PartyID = ff_identity:party(Identity),
        ContractID = ff_identity:contract(Identity),
        PartyRev = maps:get(party_revision, Params),

        PartyRevision = get_party_revision(PartyRev, PartyID),
        DomainRevision = maps:get(domain_revision, Params),
        Timestamp = maps:get(timestamp, Params),

        VS = create_varset(Params#{party_id => PartyID}),

        Terms = unwrap(party, ff_party:get_contract_terms(
            PartyID, ContractID, VS, Timestamp, PartyRevision, DomainRevision)),
        {Timestamp, PartyRevision, DomainRevision, Terms}
    end).

get_party_revision(undefined, PartyID) ->
    {ok, PartyRevision} = ff_party:get_revision(PartyID),
    PartyRevision;
get_party_revision(PartyRev, _) ->
    PartyRev.
