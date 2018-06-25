%%%
%%% Identity
%%%
%%% Essentially a contract + a number of identity claims.
%%%  * What Payment Institution? Why does it matter?
%%%
%%% We should know:
%%%  * What are the fees?
%%%  * What are the limits?
%%%  * Who will sell us e-money? This is a party + shop pair probably.
%%%  * Who will provide us withdrawals? This is a party + shop pair probably.
%%%

-module(ff_identity).

%% API

-type id(T)             :: T.
-type timestamp()       :: machinery:timestamp().
-type party()           :: ff_party:id().
-type provider()        :: ff_provider:provider().
-type contract()        :: ff_party:contract().
-type class()           :: ff_identity_class:class().
-type level()           :: ff_identity_class:level().
-type challenge_class() :: ff_identity_class:challenge_class().

-type identity() :: #{
    id           := id(_),
    party        := party(),
    provider     := provider(),
    class        := class(),
    level        := level(),
    contract     => contract(),
    challenges   => #{id(_) => challenge()}
}.

-type challenge() :: #{
    identity     := id(_),
    class        := challenge_class(),
    proofs       := [proof()],
    status       := challenge_status()
}.

-type proof() ::
    _TODO.

-type challenge_status() ::
    pending                              |
    {completed , challenge_completion()} |
    {failed    , challenge_failure()}    |
    cancelled                            .

-type challenge_completion() :: #{
    valid_until  => timestamp()
}.

-type challenge_failure() ::
    _TODO.

-type ev() ::
    {contract_set      , contract()}            |
    {level_changed     , level()}               |
    {challenge_started , id(_), challenge()}    |
    {challenge         , id(_), challenge_ev()} .

-type challenge_ev() ::
    {status_changed    , challenge_status()}.

-type outcome() ::
    [ev()].

-export_type([identity/0]).
-export_type([ev/0]).

-export([id/1]).
-export([provider/1]).
-export([party/1]).
-export([class/1]).
-export([contract/1]).

-export([is_accessible/1]).

-export([create/3]).
-export([setup_contract/1]).
-export([start_challenge/4]).

-export([challenge/2]).
-export([challenge_status/1]).

-export([poll_challenge_completion/2]).

-export([apply_event/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2, expect/2, flip/1, valid/2]).

%% Accessors

-spec id(identity()) -> id(_).
id(#{id := V}) -> V.

-spec provider(identity()) -> provider().
provider(#{provider := V}) -> V.

-spec class(identity()) -> class().
class(#{class := V})    -> V.

-spec level(identity()) -> level().
level(#{level := V})    -> V.

-spec party(identity()) -> party().
party(#{party := V})    -> V.

-spec contract(identity()) ->
    {ok, contract()} |
    {error, notfound}.

contract(V) ->
    ff_map:find(contract, V).

-spec is_accessible(identity()) ->
    {ok, accessible} |
    {error, {inaccessible, suspended | blocked}}.

is_accessible(Identity) ->
    ff_party:is_accessible(party(Identity)).

%% Constructor

-spec create(party(), provider(), class()) ->
    {ok, identity()}.

create(Party, Provider, Class) ->
    do(fun () ->
        #{
            party    => Party,
            provider => Provider,
            class    => Class,
            level    => ff_identity_class:initial_level(Class)
        }
    end).

-spec setup_contract(identity()) ->
    {ok, outcome()} |
    {error,
        invalid
    }.

setup_contract(Identity) ->
    do(fun () ->
        Class    = class(Identity),
        Contract = unwrap(ff_party:create_contract(party(Identity), #{
            payinst           => ff_provider:payinst(provider(Identity)),
            contract_template => ff_identity_class:contract_template(Class),
            contractor_level  => ff_identity_class:contractor_level(level(Identity))
        })),
        [{contract_set, Contract}]
    end).

%%

-spec start_challenge(id(_), challenge_class(), [proof()], identity()) ->
    {ok, outcome()} |
    {error,
        exists |
        {level, ff_identity_class:level()} |
        _IdentityClassError
    }.

start_challenge(ChallengeID, ChallengeClass, Proofs, Identity) ->
    do(fun () ->
        Class     = class(Identity),
        BaseLevel = ff_identity_class:base_level(ChallengeClass, Class),
        notfound  = expect(exists, flip(challenge(ChallengeID, Identity))),
        ok        = unwrap(level, valid(BaseLevel, level(Identity))),
        Challenge = unwrap(create_challenge(ChallengeID, id(Identity), ChallengeClass, Proofs)),
        [{challenge_started, ChallengeID, Challenge}]
    end).

create_challenge(_ID, IdentityID, Class, Proofs) ->
    do(fun () ->
        #{
            identity => IdentityID,
            class    => Class,
            proofs   => Proofs,
            status   => pending
        }
    end).

-spec challenge(id(_), identity()) ->
    {ok, challenge()} |
    {error, notfound}.

challenge(ChallengeID, #{challenges := Challenges}) ->
    ff_map:find(ChallengeID, Challenges).

-spec challenge_status(challenge()) ->
    challenge_status().

challenge_status(#{challenge_status := V}) ->
    V.

-spec challenge_class(challenge()) ->
    challenge_class().

challenge_class(#{class := V}) ->
    V.

-spec poll_challenge_completion(id(_), challenge()) ->
    {ok, outcome()} |
    {error,
        notfound |
        challenge_status()
    }.

poll_challenge_completion(ID, Identity) ->
    do(fun () ->
        Challenge   = unwrap(challenge(ID, Identity)),
        ok          = unwrap(valid(pending, challenge_status(Challenge))),
        TargetLevel = ff_identity_class:target_level(challenge_class(Challenge)),
        [
            {challenge, ID, {status_changed, {completed, #{}}}},
            {level_changed, TargetLevel}
        ]
    end).

%%

-spec apply_event(ev(), identity()) ->
    identity().

apply_event({contract_set, C}, Identity) ->
    Identity#{contract => C};
apply_event({level_changed, L}, Identity) ->
    Identity#{level := L};
apply_event({challenge_started, ID, C}, Identity) ->
    Cs = maps:get(challenges, Identity, #{}),
    Identity#{
        challenges => Cs#{ID => C}
    };
apply_event({challenge, ID, Ev}, Identity = #{challenges := Cs}) ->
    Identity#{
        challenges := Cs#{
            ID := apply_challenge_event(Ev, maps:get(ID, Cs))
        }
    }.

apply_challenge_event({status_changed, S}, Challenge) ->
    Challenge#{status := S}.
