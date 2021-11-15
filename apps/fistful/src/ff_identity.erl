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

-type id() :: binary().
-type name() :: binary().
-type external_id() :: id() | undefined.
-type party_id() :: ff_party:id().
-type provider_id() :: ff_provider:id().
-type contract_id() :: ff_party:contract_id().
-type blocking() :: unblocked | blocked.
-type metadata() :: ff_entity_context:md().

-define(ACTUAL_FORMAT_VERSION, 2).

-type identity_state() :: #{
    id := id(),
    name := name(),
    party := party_id(),
    provider := provider_id(),
    contract := contract_id(),
    external_id => id(),
    blocking => blocking(),
    metadata => metadata(),
    created_at => ff_time:timestamp_ms()
}.

-type identity() :: #{
    version := ?ACTUAL_FORMAT_VERSION,
    id := id(),
    name := name(),
    party := party_id(),
    provider := provider_id(),
    contract := contract_id(),
    external_id => id(),
    metadata => metadata(),
    created_at => ff_time:timestamp_ms()
}.

-type event() ::
    {created, identity()}.


-type params() :: #{
    id := id(),
    name := name(),
    party := ff_party:id(),
    provider := ff_provider:id(),
    external_id => id(),
    metadata => metadata()
}.

-type create_error() ::
    {provider, notfound}
    | {party, notfound}
    | ff_party:inaccessibility()
    | invalid.

-export_type([identity/0]).
-export_type([identity_state/0]).
-export_type([event/0]).
-export_type([id/0]).
-export_type([create_error/0]).
-export_type([params/0]).

-export([id/1]).
-export([name/1]).
-export([provider/1]).
-export([party/1]).
-export([contract/1]).
-export([external_id/1]).
-export([blocking/1]).
-export([created_at/1]).
-export([metadata/1]).

-export([is_accessible/1]).
-export([set_blocking/1]).

-export([create/1]).

-export([apply_event/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Accessors

-spec id(identity_state()) -> id().
-spec name(identity_state()) -> name().
-spec provider(identity_state()) -> provider_id().
-spec party(identity_state()) -> party_id().
-spec contract(identity_state()) -> contract_id().
-spec blocking(identity_state()) -> boolean() | undefined.
-spec external_id(identity_state()) -> external_id().
-spec created_at(identity_state()) -> ff_time:timestamp_ms() | undefined.
-spec metadata(identity_state()) -> metadata() | undefined.

id(#{id := V}) ->
    V.

name(#{name := V}) ->
    V.

provider(#{provider := V}) ->
    V.

party(#{party := V}) ->
    V.

contract(#{contract := V}) ->
    V.

blocking(Identity) ->
    maps:get(blocking, Identity, undefined).

external_id(Identity) ->
    maps:get(external_id, Identity, undefined).

created_at(Identity) ->
    maps:get(created_at, Identity, undefined).

metadata(Identity) ->
    maps:get(metadata, Identity, undefined).

-spec is_accessible(identity_state()) ->
    {ok, accessible}
    | {error, ff_party:inaccessibility()}.
is_accessible(Identity) ->
    ff_party:is_accessible(party(Identity)).

-spec set_blocking(identity_state()) -> identity_state().
set_blocking(Identity) ->
    Blocking =
        case {ok, accessible} =:= is_accessible(Identity) of
            true ->
                unblocked;
            false ->
                blocked
        end,
    maps:put(blocking, Blocking, Identity).

%% Constructor

-spec create(params()) ->
    {ok, [event()]}
    | {error, create_error()}.
create(Params = #{id := ID, name := Name, party := Party, provider := ProviderID}) ->
    do(fun() ->
        accessible = unwrap(party, ff_party:is_accessible(Party)),
        Provider = unwrap(provider, ff_provider:get(ProviderID)),
        Contract = unwrap(
            ff_party:create_contract(Party, #{
                payinst => ff_provider:payinst(Provider),
                contract_template => ff_provider:contract_template(Provider),
                contractor_level => ff_provider:contractor_level(Provider)
            })
        ),
        [
            {created,
                genlib_map:compact(#{
                    version => ?ACTUAL_FORMAT_VERSION,
                    id => ID,
                    name => Name,
                    party => Party,
                    provider => ProviderID,
                    contract => Contract,
                    created_at => ff_time:now(),
                    external_id => maps:get(external_id, Params, undefined),
                    metadata => maps:get(metadata, Params, undefined)
            })}
        ]
    end).

%%

-spec apply_event(event(), ff_maybe:maybe(identity_state())) -> identity_state().
apply_event({created, Identity}, undefined) ->
    Identity;
apply_event({level_changed, _L}, Identity) ->
    Identity;
apply_event({effective_challenge_changed, _ID}, Identity) ->
    Identity;
apply_event({{challenge, _ID}, _Ev}, Identity) ->
    Identity.
