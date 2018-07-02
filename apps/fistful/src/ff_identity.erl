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
    level        => level(),
    contract     => contract(),
    challenges   => #{id(_) => challenge()}
}.

-type challenge() ::
    ff_identity_challenge:challenge().

-type ev() ::
    {created           , identity()}                        |
    {contract_set      , contract()}                        |
    {level_changed     , level()}                           |
    {challenge         , id(_), ff_identity_challenge:ev()} .

-type outcome() ::
    [ev()].

-export_type([identity/0]).
-export_type([ev/0]).

-export([id/1]).
-export([provider/1]).
-export([party/1]).
-export([class/1]).
-export([contract/1]).
-export([challenge/2]).

-export([is_accessible/1]).

-export([create/4]).
-export([setup_contract/1]).

-export([start_challenge/4]).
-export([poll_challenge_completion/2]).

-export([collapse_events/1]).
-export([apply_events/2]).
-export([apply_event/2]).

% -export([hydrate/1]).
% -export([dehydrate/1]).

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
    ff_map:result(contract()).

contract(V) ->
    ff_map:find(contract, V).

-spec challenge(id(_), identity()) ->
    ff_map:result(challenge()).

challenge(ChallengeID, Identity) ->
    ff_map:find(ChallengeID, maps:get(challenges, Identity, #{})).

-spec is_accessible(identity()) ->
    {ok, accessible} |
    {error, {inaccessible, suspended | blocked}}.

is_accessible(Identity) ->
    ff_party:is_accessible(party(Identity)).

%% Constructor

-spec create(id(_), party(), provider(), class()) ->
    {ok, outcome()}.

create(ID, Party, Provider, Class) ->
    do(fun () ->
        [
            {created, #{
                id       => ID,
                party    => Party,
                provider => Provider,
                class    => Class
            }},
            {level_changed,
                ff_identity_class:initial_level(Class)
            }
        ]
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

-spec start_challenge(id(_), challenge_class(), [ff_identity_challenge:proof()], identity()) ->
    {ok, outcome()} |
    {error,
        exists |
        {level, ff_identity_class:level()} |
        _CreateChallengeError
    }.

start_challenge(ChallengeID, ChallengeClass, Proofs, Identity) ->
    do(fun () ->
        BaseLevel = ff_identity_class:base_level(ChallengeClass),
        notfound  = expect(exists, flip(challenge(ChallengeID, Identity))),
        ok        = unwrap(level, valid(BaseLevel, level(Identity))),
        Challenge = unwrap(ff_identity_challenge:create(id(Identity), ChallengeClass, Proofs)),
        [{challenge_started, ChallengeID, Challenge}]
    end).

-spec poll_challenge_completion(id(_), identity()) ->
    {ok, outcome()} |
    {error,
        notfound |
        ff_identity_challenge:status()
    }.

poll_challenge_completion(ID, Identity) ->
    do(fun () ->
        Challenge = unwrap(challenge(ID, Identity)),
        case unwrap(ff_identity_challenge:poll_completion(Challenge)) of
            [] ->
                [];
            Events = [_ | _] ->
                TargetLevel = ff_identity_class:target_level(ff_identity_challenge:class(Challenge)),
                [{challenge, ID, Ev} || Ev <- Events] ++
                    [{level_changed, TargetLevel}]
        end
    end).

%%

-spec collapse_events([ev(), ...]) ->
    identity().

collapse_events(Evs) when length(Evs) > 0 ->
    apply_events(Evs, undefined).

-spec apply_events([ev()], undefined | identity()) ->
    undefined | identity().

apply_events(Evs, Identity) ->
    lists:foldl(fun apply_event/2, Identity, Evs).

-spec apply_event(ev(), undefined | identity()) ->
    identity().

apply_event({created, Identity}, undefined) ->
    Identity;
apply_event({contract_set, C}, Identity) ->
    Identity#{contract => C};
apply_event({level_changed, L}, Identity) ->
    Identity#{level => L};
apply_event({challenge_started, ID, C}, Identity) ->
    Cs = maps:get(challenges, Identity, #{}),
    Identity#{
        challenges => Cs#{ID => C}
    };
apply_event({challenge, ID, Ev}, Identity = #{challenges := Cs}) ->
    Identity#{
        challenges := Cs#{
            ID := ff_identity_challenge:apply_event(Ev, maps:get(ID, Cs))
        }
    }.

% -spec dehydrate(ev()) ->
%     term().
