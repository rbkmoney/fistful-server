%%%
%%% Identity challenge activity
%%%

-module(ff_identity_challenge).

%% API

-type id(T)       :: T.
-type timestamp() :: machinery:timestamp().
-type class()     :: ff_identity_class:challenge_class().
-type master_id() :: id(binary()).
-type claim_id()  :: id(binary()).

-type challenge() :: #{
    class     := class(),
    claimant  := id(_),
    proofs    := [proof()],
    master_id := master_id(),
    claim_id  := claim_id(),
    status    => status()
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

-type ev() ::
    {created, challenge()} |
    {status_changed, status()}.

-type outcome() ::
    [ev()].

-export_type([challenge/0]).
-export_type([ev/0]).

-export([claimant/1]).
-export([status/1]).
-export([class/1]).
-export([proofs/1]).
-export([resolution/1]).
-export([claim_id/1]).
-export([master_id/1]).

-export([create/3]).
-export([poll_completion/1]).

-export([apply_events/2]).
-export([apply_event/2]).

-export([dehydrate/1]).
-export([hydrate/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2, valid/2]).

%%

-spec status(challenge()) ->
    status().

status(#{status := V}) ->
    V.

-spec claimant(challenge()) ->
    id(_).

claimant(#{claimant := V}) ->
    V.

-spec class(challenge()) ->
    class().

class(#{class := V}) ->
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

-spec create(id(_), class(), [proof()]) ->
    {ok, outcome()} |
    {error,
        {proof, notfound | insufficient} |
        _StartError
    }.

create(Claimant, Class, Proofs) ->
    do(fun () ->
        TargetLevel = ff_identity_class:target_level(Class),
        MasterID = unwrap(deduce_identity_id(Proofs)),
        ClaimID  = unwrap(create_claim(MasterID, TargetLevel, Claimant, Proofs)),
        [
            {created, #{
                class     => Class,
                claimant  => Claimant,
                proofs    => Proofs,
                master_id => MasterID,
                claim_id  => ClaimID
            }},
            {status_changed,
                pending
            }
        ]
    end).

-spec poll_completion(challenge()) ->
    {ok, outcome()} |
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

-spec apply_events([ev()], undefined | challenge()) ->
    undefined | challenge().

apply_events(Evs, Challenge) ->
    lists:foldl(fun apply_event/2, Challenge, Evs).

-spec apply_event(ev(), undefined | challenge()) ->
    challenge().

apply_event({created, Challenge}, undefined) ->
    Challenge;
apply_event({status_changed, S}, Challenge) ->
    Challenge#{status => S}.

-spec dehydrate(ev()) ->
    term().

-spec hydrate(term(), undefined | challenge()) ->
    ev().

dehydrate(Ev) ->
    Ev.

hydrate(Ev, _) ->
    Ev.

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
