%%%
%%% Instrument
%%%
%%% TODOs
%%%
%%%  - We must consider withdrawal provider terms ensure that the provided
%%%    resource is ok to withdraw to.
%%%

-module(ff_instrument).

-type id()        :: binary().
-type name()      :: binary().
-type resource(T) :: T.
-type account()   :: ff_account:account().
-type identity()  :: ff_identity:id().
-type currency()  :: ff_currency:id().
-type status()    ::
    unauthorized |
    authorized.

-type instrument(T) :: #{
    account  := account() | undefined,
    resource := resource(T),
    name     := name(),
    status   := status()
}.

-type event(T) ::
    {created, instrument(T)} |
    {account, ff_account:ev()} |
    {status_changed, status()}.

-export_type([id/0]).
-export_type([instrument/1]).
-export_type([status/0]).
-export_type([resource/1]).
-export_type([event/1]).

-export([account/1]).

-export([id/1]).
-export([name/1]).
-export([identity/1]).
-export([currency/1]).
-export([resource/1]).
-export([status/1]).

-export([create/5]).
-export([authorize/1]).

-export([is_accessible/1]).

-export([apply_event/2]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Accessors

-spec account(instrument(_)) ->
    account().

account(#{account := V}) ->
    V.

-spec id(instrument(_)) ->
    id().
-spec name(instrument(_)) ->
    binary().
-spec identity(instrument(_)) ->
    identity().
-spec currency(instrument(_)) ->
    currency().
-spec resource(instrument(T)) ->
    resource(T).
-spec status(instrument(_)) ->
    status().

id(Instrument) ->
    ff_account:id(account(Instrument)).
name(#{name := V}) ->
    V.
identity(Instrument) ->
    ff_account:identity(account(Instrument)).
currency(Instrument) ->
    ff_account:currency(account(Instrument)).
resource(#{resource := V}) ->
    V.
status(#{status := V}) ->
    V.

%%

-spec create(id(), identity(), binary(), currency(), resource(T)) ->
    {ok, [event(T)]} |
    {error, _WalletError}.

create(ID, IdentityID, Name, CurrencyID, Resource) ->
    do(fun () ->
        Identity = ff_identity_machine:identity(unwrap(identity, ff_identity_machine:get(IdentityID))),
        Currency = unwrap(currency, ff_currency:get(CurrencyID)),
        Events = unwrap(ff_account:create(ID, Identity, Currency)),
        [{created, #{name => Name, resource => Resource}}] ++
        [{account, Ev} || Ev <- Events] ++
        [{status_changed, unauthorized}]
    end).

-spec authorize(instrument(T)) ->
    {ok, [event(T)]} |
    {error, _TODO}.

authorize(#{status := unauthorized}) ->
    % TODO
    %  - Do the actual authorization
    {ok, [{status_changed, authorized}]};
authorize(#{status := authorized}) ->
    {ok, []}.

-spec is_accessible(instrument(_)) ->
    {ok, accessible} |
    {error, ff_party:inaccessibility()}.

is_accessible(Instrument) ->
    ff_account:is_accessible(account(Instrument)).

%%

-spec apply_event(event(T), ff_maybe:maybe(instrument(T))) ->
    instrument(T).

apply_event({created, Instrument}, undefined) ->
    Instrument;
apply_event({status_changed, S}, Instrument) ->
    Instrument#{status => S};
apply_event({account, Ev}, Instrument = #{account := Account}) ->
    Instrument#{account => ff_account:apply_event(Ev, Account)};
apply_event({account, Ev}, Instrument) ->
    apply_event({account, Ev}, Instrument#{account => undefined}).
