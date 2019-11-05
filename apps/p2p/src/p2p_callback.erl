-module(p2p_callback).

-define(ACTUAL_FORMAT_VERSION, 1).

-type tag() :: binary().
-type payload() :: binary().

-opaque callback() :: #{
    version := ?ACTUAL_FORMAT_VERSION,
    tag := tag(),
    status => status(),
    payload => payload(),
    response => response()
}.

-type response() :: #{
    payload => payload()
}.

-type params() :: #{
    tag := tag()
}.

-type process_params() :: #{
    tag := tag(),
    payload := payload()
}.

-type status() ::
    pending |
    succeeded.

-type legacy_event() :: any().
-type event() ::
    {created, callback()} |
    {succeeded, response()} |
    {status_changed, status()}.

-export_type([tag/0]).
-export_type([event/0]).
-export_type([response/0]).
-export_type([status/0]).
-export_type([callback/0]).
-export_type([params/0]).
-export_type([process_params/0]).

%% Accessors

-export([tag/1]).
-export([status/1]).
-export([response/1]).

%% API

-export([create/1]).
-export([is_active/1]).
-export([is_finished/1]).
-export([process_callback/1]).
-export([set_callback_payload/2]).

%% Event source

-export([apply_event/2]).
-export([maybe_migrate/1]).

%% Internal types

-type process_result() :: [event()].

%% Accessors

-spec tag(callback()) -> tag().
tag(#{tag := V}) ->
    V.

-spec status(callback()) -> status().
status(#{status := V}) ->
    V.

-spec response(callback()) -> response() | undefined.
response(C) ->
    maps:get(response, C, undefined).

%% API

-spec create(params()) ->
    {ok, process_result()}.

create(#{tag := Tag}) ->
    Callback = #{
        version => ?ACTUAL_FORMAT_VERSION,
        tag => Tag
    },
    {ok, [{created, Callback}, {status_changed, pending}]}.

%% Сущность в настоящий момент нуждается в передаче ей управления для совершения каких-то действий
-spec is_active(callback()) -> boolean().
is_active(#{status := succeeded}) ->
    false;
is_active(#{status := pending}) ->
    true.

%% Сущность приняла статус, который не будет меняться без внешних воздействий.
-spec is_finished(callback()) -> boolean().
is_finished(#{status := succeeded}) ->
    true;
is_finished(#{status := pending}) ->
    false.

-spec process_callback(callback()) ->
    {response(), process_result()}.
process_callback(Callback) ->
    case status(Callback) of
        pending ->
            % TODO add here p2p adapter call
            Response = #{payload => <<"Test payload">>},
            {Response, [
                {succeeded, Response},
                {status_changed, succeeded}
            ]};
        succeeded ->
            Response = response(Callback),
            {Response, []}
    end.

-spec set_callback_payload(payload(), callback()) ->
    callback().
set_callback_payload(Payload, Callback) ->
    Callback#{payload => Payload}.

%% Internals

-spec update_status(status(), callback()) -> callback().
update_status(Status, Callback) ->
    Callback#{status => Status}.

-spec update_response(response(), callback()) -> callback().
update_response(Response, Callback) ->
    Callback#{response => Response}.

%% Events utils

-spec apply_event(event() | legacy_event(), callback() | undefined) ->
    callback().
apply_event(Ev, T) ->
    apply_event_(maybe_migrate(Ev), T).

-spec apply_event_(event(), callback() | undefined) ->
    callback().
apply_event_({created, T}, undefined) ->
    T;
apply_event_({status_changed, S}, T) ->
    update_status(S, T);
apply_event_({succeeded, R}, T) ->
    update_response(R, T).

-spec maybe_migrate(event() | legacy_event()) ->
    event().
maybe_migrate(Ev) ->
    Ev.
