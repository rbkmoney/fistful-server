-module(p2p_user_interaction).

-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_p2p_adapter_thrift.hrl").
-include_lib("damsel/include/dmsl_user_interaction_thrift.hrl").

-define(ACTUAL_FORMAT_VERSION, 1).

-type id() :: binary().

-opaque user_interaction() :: #{
    version := ?ACTUAL_FORMAT_VERSION,
    id := id(),
    content := content(),
    status => status()
}.
-type intent() :: finish | {create, content()}.
-type content() :: redirect() | receipt() | crypto() | qr_code().

-type redirect() :: #{
    type    := redirect,
    content := redirect_get() | redirect_post()
}.

-type receipt() :: #{
    type       := payment_terminal_receipt,
    payment_id := payment_id(),
    timestamp  := timestamp()
}.

-type crypto() :: #{
    type           := crypto_currency_transfer_request,
    crypto_address := crypto_address(),
    crypto_cash    := crypto_cash()
}.

-type qr_code() :: #{
    type    := qr_code_show_request,
    payload := qr_code_payload()
}.

-type redirect_get() :: {get, uri()}.
-type redirect_post() :: {post, uri(), form()}.
-type uri() :: binary().
-type form() :: #{binary() => template()}.
-type template() :: binary().

-type payment_id() :: binary().
-type timestamp() :: binary().

-type crypto_address() :: binary().
-type crypto_cash() :: {crypto_amount(), crypto_symbolic_code()}.
-type crypto_amount() :: genlib_rational:t().
-type crypto_symbolic_code() :: binary().

-type qr_code_payload() :: binary().

-type params() :: #{
    id := id(),
    content := content()
}.

-type status() ::
    pending |
    finished.

-type legacy_event() :: any().
-type event() ::
    {created, user_interaction()} |
    {status_changed, status()}.

-export_type([id/0]).
-export_type([event/0]).
-export_type([status/0]).
-export_type([user_interaction/0]).
-export_type([params/0]).
-export_type([intent/0]).
-export_type([content/0]).

%% Accessors

-export([id/1]).
-export([status/1]).

%% API

-export([create/1]).
-export([is_active/1]).
-export([is_finished/1]).
-export([finish/1]).

%% Event source

-export([apply_event/2]).
-export([maybe_migrate/1]).

%% Internal types

-type process_result() :: [event()].

%% Accessors

-spec id(user_interaction()) -> id().
id(#{id := V}) ->
    V.

-spec status(user_interaction()) -> status().
status(#{status := V}) ->
    V.

%% API

-spec create(params()) ->
    {ok, process_result()}.

create(#{id := ID, content := Content}) ->
    UserInteraction = #{
        version => ?ACTUAL_FORMAT_VERSION,
        id => ID,
        content => Content
    },
    {ok, [{created, UserInteraction}, {status_changed, pending}]}.

%% Сущность в настоящий момент нуждается в передаче ей управления для совершения каких-то действий
-spec is_active(user_interaction()) -> boolean().
is_active(#{status := finished}) ->
    false;
is_active(#{status := pending}) ->
    true.

%% Сущность приняла статус, который не будет меняться без внешних воздействий.
-spec is_finished(user_interaction()) -> boolean().
is_finished(#{status := finished}) ->
    true;
is_finished(#{status := pending}) ->
    false.

-spec finish(user_interaction()) ->
    process_result().
finish(#{status := pending}) ->
    [{status_changed, finished}];
finish(#{status := finished}) ->
    [].

%% Internals

-spec update_status(status(), user_interaction()) -> user_interaction().
update_status(Status, UserInteraction) ->
    UserInteraction#{status => Status}.

%% Events utils

-spec apply_event(event() | legacy_event(), user_interaction() | undefined) ->
    user_interaction().
apply_event(Ev, T) ->
    apply_event_(maybe_migrate(Ev), T).

-spec apply_event_(event(), user_interaction() | undefined) ->
    user_interaction().
apply_event_({created, T}, undefined) ->
    T;
apply_event_({status_changed, S}, T) ->
    update_status(S, T).

-spec maybe_migrate(event() | legacy_event()) ->
    event().
maybe_migrate(Ev) ->
    Ev.
