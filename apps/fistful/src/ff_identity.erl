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

-type id()              :: binary().
-type external_id()     :: id() | undefined.
-type party()           :: ff_party:id().
-type provider()        :: ff_provider:id().
-type contract()        :: ff_party:contract_id().
-type class()           :: ff_identity_class:id().
-type level()           :: ff_identity_class:level_id().
-type challenge_class() :: ff_identity_class:challenge_class_id().
-type challenge_id()    :: id().
-type blocked()         :: boolean().

-type identity() :: #{
    id           := id(),
    party        := party(),
    provider     := provider(),
    class        := class(),
    contract     := contract(),
    level        => level(),
    challenges   => #{challenge_id() => challenge()},
    effective    => challenge_id(),
    external_id  => id(),
    blocked      => blocked()
}.

-type challenge() ::
    ff_identity_challenge:challenge().

-type event() ::
    {created           , identity()}                                 |
    {level_changed     , level()}                                    |
    {effective_challenge_changed, challenge_id()}                    |
    {{challenge        , challenge_id()}, ff_identity_challenge:ev()}.

-export_type([identity/0]).
-export_type([event/0]).
-export_type([id/0]).

-export([id/1]).
-export([provider/1]).
-export([party/1]).
-export([class/1]).
-export([level/1]).
-export([contract/1]).
-export([challenges/1]).
-export([challenge/2]).
-export([effective_challenge/1]).
-export([external_id/1]).
-export([blocked/1]).

-export([is_accessible/1]).
-export([set_blocked/1]).

-export([create/5]).

-export([start_challenge/4]).
-export([poll_challenge_completion/2]).

-export([apply_event/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2, expect/2, flip/1, valid/2]).

%% Accessors

-spec id(identity()) ->
    id().
-spec provider(identity()) ->
    provider().
-spec class(identity()) ->
    class().
-spec party(identity()) ->
    party().
-spec contract(identity()) ->
    contract().
-spec blocked(identity()) ->
    boolean() | undefined.
-spec level(identity()) ->
    level() | undefined.
-spec challenges(identity()) ->
    #{challenge_id() => challenge()}.
-spec effective_challenge(identity()) ->
    ff_map:result(challenge_id()).
-spec challenge(challenge_id(), identity()) ->
    ff_map:result(challenge()).
-spec external_id(identity()) ->
    external_id().

id(#{id := V}) ->
    V.

provider(#{provider := V}) ->
    V.

class(#{class := V}) ->
    V.

party(#{party := V}) ->
    V.

contract(#{contract := V}) ->
    V.

blocked(Identity) ->
    maps:get(blocked, Identity, undefined).

level(Identity) ->
    maps:get(level, Identity, undefined).

challenges(Identity) ->
    maps:get(challenges, Identity, #{}).

effective_challenge(Identity) ->
    ff_map:find(effective, Identity).

challenge(ChallengeID, Identity) ->
    ff_map:find(ChallengeID, challenges(Identity)).

external_id(Identity) ->
    maps:get(external_id, Identity, undefined).

-spec is_accessible(identity()) ->
    {ok, accessible} |
    {error, ff_party:inaccessibility()}.

is_accessible(Identity) ->
    ff_party:is_accessible(party(Identity)).


-spec set_blocked(identity()) -> identity().

set_blocked(Identity) ->
    Blocked = {ok, accessible} =/= is_accessible(Identity),
    maps:put(blocked, Blocked, Identity).


%% Constructor

-spec create(id(), party(), provider(), class(), external_id()) ->
    {ok, [event()]} |
    {error,
        {provider, notfound} |
        {identity_class, notfound} |
        ff_party:inaccessibility() |
        invalid
    }.

create(ID, Party, ProviderID, ClassID, ExternalID) ->
    do(fun () ->
        Provider = unwrap(provider, ff_provider:get(ProviderID)),
        Class = unwrap(identity_class, ff_provider:get_identity_class(ClassID, Provider)),
        LevelID = ff_identity_class:initial_level(Class),
        {ok, Level} = ff_identity_class:level(LevelID, Class),
        Contract = unwrap(ff_party:create_contract(Party, #{
            payinst           => ff_provider:payinst(Provider),
            contract_template => ff_identity_class:contract_template(Class),
            contractor_level  => ff_identity_class:contractor_level(Level)
        })),
        [
            {created, add_external_id(ExternalID, #{
                id       => ID,
                party    => Party,
                provider => ProviderID,
                class    => ClassID,
                contract => Contract
            })},
            {level_changed,
                LevelID
            }
        ]
    end).

%%

-spec start_challenge(challenge_id(), challenge_class(), [ff_identity_challenge:proof()], identity()) ->
    {ok, [event()]} |
    {error,
        exists |
        {challenge_class, notfound} |
        {level, ff_identity_class:level()} |
        {proof, notfound | insufficient} |
        pending |
        conflict
    }.

start_challenge(ChallengeID, ChallengeClassID, Proofs, Identity) ->
    do(fun () ->
        notfound = expect(exists, flip(challenge(ChallengeID, Identity))),
        IdentityClass = get_identity_class(Identity),
        ChallengeClass = unwrap(challenge_class, ff_identity_class:challenge_class(
            ChallengeClassID,
            IdentityClass
        )),
        ok = unwrap(level, valid(ff_identity_class:base_level(ChallengeClass), level(Identity))),
        Events = unwrap(ff_identity_challenge:create(
            ChallengeID,
            id(Identity),
            provider(Identity),
            class(Identity),
            ChallengeClassID,
            Proofs
        )),
        [{{challenge, ChallengeID}, Ev} || Ev <- Events]
    end).

-spec poll_challenge_completion(challenge_id(), identity()) ->
    {ok, [event()]} |
    {error,
        notfound |
        ff_identity_challenge:status()
    }.

poll_challenge_completion(ChallengeID, Identity) ->
    do(fun () ->
        Challenge = unwrap(challenge(ChallengeID, Identity)),
        case unwrap(ff_identity_challenge:poll_completion(Challenge)) of
            [] ->
                [];
            Events = [_ | _] ->
                Contract  = contract(Identity),
                IdentityClass = get_identity_class(Identity),
                ChallengeClass = get_challenge_class(Challenge, Identity),
                TargetLevelID = ff_identity_class:target_level(ChallengeClass),
                {ok, Level} = ff_identity_class:level(TargetLevelID, IdentityClass),
                ok = unwrap(ff_party:change_contractor_level(
                    party(Identity),
                    Contract,
                    ff_identity_class:contractor_level(Level)
                )),
                [{{challenge, ChallengeID}, Ev} || Ev <- Events] ++
                [
                    {level_changed, TargetLevelID},
                    {effective_challenge_changed, ChallengeID}
                ]
        end
    end).

get_provider(Identity) ->
    {ok, V} = ff_provider:get(provider(Identity)), V.

get_identity_class(Identity) ->
    {ok, V} = ff_provider:get_identity_class(class(Identity), get_provider(Identity)), V.

get_challenge_class(Challenge, Identity) ->
    {ok, V} = ff_identity_class:challenge_class(
        ff_identity_challenge:class(Challenge),
        get_identity_class(Identity)
    ),
    V.

add_external_id(undefined, Event) ->
    Event;
add_external_id(ExternalID, Event) ->
    Event#{external_id => ExternalID}.

%%

-spec apply_event(event(), ff_maybe:maybe(identity())) ->
    identity().

apply_event({created, Identity}, undefined) ->
    Identity;
apply_event({level_changed, L}, Identity) ->
    Identity#{level => L};
apply_event({effective_challenge_changed, ID}, Identity) ->
    Identity#{effective => ID};
apply_event({{challenge, ID}, Ev}, Identity) ->
    with_challenges(
        fun (Cs) ->
            with_challenge(
                ID,
                fun (C) -> ff_identity_challenge:apply_event(Ev, C) end,
                Cs
            )
        end,
        Identity
    ).

with_challenges(Fun, Identity) ->
    maps:update_with(challenges, Fun, maps:merge(#{challenges => #{}}, Identity)).

with_challenge(ID, Fun, Challenges) ->
    maps:update_with(ID, Fun, maps:merge(#{ID => undefined}, Challenges)).
