%%%
%%% Identity challenge activity
%%%
%%% TODOs
%%%
%%%  - `ProviderID` + `IdentityClassID` + `ChallengeClassID` easily replaceable
%%%    with a _single_ identifier if we drop strictly hierarchical provider
%%%    definition.
%%%

-module(ff_identity_challenge).

%% API

-type id(T)       :: T.
-type claimant() :: id(binary()).
-type timestamp() :: machinery:timestamp().
-type provider()     :: ff_provider:id().
-type identity_class() :: ff_identity_class:id().
-type challenge_class_id() :: ff_identity_class:challenge_class_id().
-type master_id() :: id(binary()).
-type claim_id()  :: id(binary()).

-type challenge() :: #{
    % Are fields here actually optional?
    id              := id(_),
    claimant        => claimant(),
    provider        => provider(),
    identity_class  => identity_class(),
    challenge_class := challenge_class_id(),
    proofs          := [proof()],
    master_id       => master_id(),
    claim_id        => claim_id(),
    status          := status()
}.

-type proof() ::
    {proof_type(), identdoc_token()}.

-type proof_type() ::
    rus_domestic_passport |
    rus_retiree_insurance_cert.

-type identdoc_token() ::
    binary().

-type status() ::
    pending                    |
    {completed , completion()} |
    {failed    , failure()}    |
    cancelled                  .

-type completion() :: #{
    resolution  := resolution(),
    valid_until => timestamp()
}.

-type resolution() ::
    approved |
    denied   .

-type failure() ::
    _TODO.

-type event() ::
    {created, challenge()} |
    {status_changed, status()}.

-type create_error() ::
    {proof, notfound | insufficient} |
    pending |
    conflict.

-export_type([challenge/0]).
-export_type([event/0]).
-export_type([create_error/0]).
-export_type([proof/0]).
-export_type([id/1]).
-export_type([status/0]).

-export([id/1]).
-export([claimant/1]).
-export([status/1]).
-export([class/1]).
-export([proofs/1]).
-export([resolution/1]).
-export([claim_id/1]).
-export([master_id/1]).

-export([create/6]).
-export([poll_completion/1]).

-export([apply_event/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, valid/2]).

%%

-spec id(challenge()) ->
    id(_).

id(#{id := V}) ->
    V.

-spec status(challenge()) ->
    status() | undefined.

status(Challenge) ->
    maps:get(status, Challenge, undefined).

-spec claimant(challenge()) ->
    claimant().

claimant(#{claimant := V}) ->
    V.

-spec class(challenge()) ->
    challenge_class_id().

class(#{challenge_class := V}) ->
    V.

-spec proofs(challenge()) ->
    [proof()].

proofs(#{proofs := V}) ->
    V.

-spec resolution(challenge()) ->
    {ok, resolution()} |
    {error, undefined} .

resolution(Challenge) ->
    case status(Challenge) of
        {completed, #{resolution := Resolution}} ->
            {ok, Resolution};
        _Status ->
            {error, undefined}
    end.

-spec master_id(challenge()) ->
    id(_).

master_id(#{master_id := V}) ->
    V.

-spec claim_id(challenge()) ->
    id(_).

claim_id(#{claim_id := V}) ->
    V.

%%

-spec create(id(_), claimant(), provider(), identity_class(), challenge_class_id(), [proof()]) ->
    {ok, [event()]} |
    {error, create_error()}.

create(ID, Claimant, ProviderID, IdentityClassID, ChallengeClassID, Proofs) ->
    do(fun () ->
        {ok, Provider} = ff_provider:get(ProviderID),
        {ok, IdentityClass} = ff_provider:get_identity_class(IdentityClassID, Provider),
        {ok, ChallengeClass} = ff_identity_class:challenge_class(ChallengeClassID, IdentityClass),
        TargetLevelID = ff_identity_class:target_level(ChallengeClass),
        {ok, TargetLevel} = ff_identity_class:level(TargetLevelID, IdentityClass),
        MasterID = unwrap(deduce_identity_id(Proofs)),
        ClaimID = unwrap(create_claim(MasterID, TargetLevel, Claimant, Proofs)),
        [
            {created, #{
                id              => ID,
                claimant        => Claimant,
                provider        => ProviderID,
                identity_class  => IdentityClassID,
                challenge_class => ChallengeClassID,
                proofs          => Proofs,
                master_id       => MasterID,
                claim_id        => ClaimID
            }},
            {status_changed,
                pending
            }
        ]
    end).

-spec poll_completion(challenge()) ->
    {ok, [event()]} |
    {error,
        notfound |
        status()
    }.

poll_completion(Challenge) ->
    do(fun () ->
        ok = unwrap(valid(pending, status(Challenge))),
        Status = unwrap(get_claim_status(claim_id(Challenge))),
        case Status of
            created ->
                [];
            approved ->
                [{status_changed, {completed, #{resolution => approved}}}];
            denied ->
                [{status_changed, {completed, #{resolution => denied}}}];
            {failed, Failure} ->
                [{status_changed, {failed, Failure}}];
            cancelled ->
                [{status_changed, cancelled}]
        end
    end).

%%

-spec apply_event(event(), ff_maybe:maybe(challenge())) ->
    challenge().

apply_event({created, Challenge}, undefined) ->
    Challenge;
apply_event({status_changed, S}, Challenge) ->
    Challenge#{status => S}.

%%

-include_lib("id_proto/include/id_proto_identification_thrift.hrl").

deduce_identity_id(Proofs) ->
    case call('GetIdentityID', [encode({list, identity_document}, Proofs)]) of
        {ok, IdentityID} ->
            {ok, decode(identity_id, IdentityID)};
        {exception, #identity_IdentityDocumentNotFound{}} ->
            {error, {proof, notfound}};
        {exception, #identity_InsufficientIdentityDocuments{}} ->
            {error, {proof, insufficient}}
    end.

create_claim(MasterID, TargetLevel, Claimant, Proofs) ->
    case call('CreateClaim', [encode(identity_claim_params, {MasterID, TargetLevel, Claimant, Proofs})]) of
        {ok, #identity_IdentityClaim{id = ID}} ->
            {ok, decode(identity_claim_id, ID)};
        {exception, #identity_ClaimPending{}} ->
            {error, pending};
        {exception, #identity_InsufficientIdentityDocuments{}} ->
            {error, {proof, insufficient}};
        {exception, #identity_IdentityOwnershipConflict{}} ->
            {error, conflict};
        {exception, Unexpected} ->
            error(Unexpected)
    end.

get_claim_status(ClaimID) ->
    case call('GetClaim', [encode(identity_claim_id, ClaimID)]) of
        {ok, #identity_IdentityClaim{status = Status}} ->
            {ok, decode(identity_claim_status, Status)};
        {exception, #identity_ClaimNotFound{}} ->
            {error, notfound}
    end.

encode(identity_claim_params, {MasterID, TargetLevel, Claimant, Proofs}) ->
    #identity_IdentityClaimParams{
        identity_id  = encode(identity_id, MasterID),
        target_level = encode(level, ff_identity_class:contractor_level(TargetLevel)),
        claimant     = encode(claimant, Claimant),
        proof        = encode({list, identity_document}, Proofs)
    };
encode(level, Level) ->
    % TODO
    Level;

encode(identity_document, {Type, Token}) ->
    #identity_IdentityDocument{
        type  = encode(identity_document_type, Type),
        token = encode(string, Token)
    };
encode(identity_document_type, rus_domestic_passport) ->
    {rus_domestic_passport, #identity_RUSDomesticPassport{}};
encode(identity_document_type, rus_retiree_insurance_cert) ->
    {rus_retiree_insurance_cert, #identity_RUSRetireeInsuranceCert{}};

encode(identity_claim_id, V) ->
    encode(string, V);
encode(identity_id, V) ->
    encode(string, V);
encode(claimant, V) ->
    encode(string, V);

encode({list, T}, V) when is_list(V) ->
    [encode(T, E) || E <- V];
encode(string, V) when is_binary(V) ->
    V.

%%

decode(identity_claim_status, {created, _}) ->
    created;
decode(identity_claim_status, {review, _}) ->
    review;
decode(identity_claim_status, {approved, _}) ->
    approved;
decode(identity_claim_status, {denied, _}) ->
    denied;
decode(identity_claim_status, {cancelled, _}) ->
    cancelled;
decode(identity_claim_status, {failed, Failure}) ->
    {failed, Failure};

decode(identity_claim_id, V) ->
    decode(string, V);
decode(identity_id, V) ->
    decode(string, V);

decode(string, V) when is_binary(V) ->
    V.

%%

call(Function, Args) ->
    % TODO
    %  - Ideally, we should provide `Client` here explicitly.
    Service = {id_proto_identification_thrift, 'Identification'},
    ff_woody_client:call(identification, {Service, Function, Args}).
