-module(ff_withdrawal_callback).

-define(ACTUAL_FORMAT_VERSION, 1).

%% Types

-type tag() :: binary().
-type payload() :: binary().

-type params() :: #{
    tag := tag()
}.

-type process_params() :: #{
    tag := tag(),
    payload := payload()
}.

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

-type status() ::
    pending
    | succeeded.

%%

-type legacy_event() :: any().
-type event() ::
    {created, callback()}
    | {finished, response()}
    | {status_changed, status()}.

-type process_result() :: [event()].

%%

-export_type([
    tag/0,
    payload/0,
    params/0,
    process_params/0,
    callback/0,
    response/0,
    status/0,
    event/0
]).

%% Accessors

-export([
    tag/1,
    status/1,
    response/1
]).

%% API

-export([
    create/1,
    process_response/2
]).

%% Events

-export([
    apply_event/2,
    maybe_migrate/1
]).

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

-spec create(params()) -> {ok, process_result()}.
create(#{tag := Tag}) ->
    Callback = #{
        version => ?ACTUAL_FORMAT_VERSION,
        tag => Tag
    },
    {ok, [{created, Callback}, {status_changed, pending}]}.

-spec process_response(response(), callback()) -> process_result().
process_response(Response, Callback) ->
    case status(Callback) of
        pending ->
            [
                {finished, Response},
                {status_changed, succeeded}
            ]
    end.

%% Utils

-spec update_status(status(), callback()) -> callback().
update_status(Status, Callback) ->
    Callback#{status => Status}.

-spec update_response(response(), callback()) -> callback().
update_response(Response, Callback) ->
    Callback#{response => Response}.

%% Events

-spec apply_event(event() | legacy_event(), callback() | undefined) -> callback().
apply_event(Ev, T) ->
    apply_event_(maybe_migrate(Ev), T).

-spec apply_event_(event(), callback() | undefined) -> callback().
apply_event_({created, T}, undefined) ->
    T;
apply_event_({status_changed, S}, T) ->
    update_status(S, T);
apply_event_({finished, R}, T) ->
    update_response(R, T).

-spec maybe_migrate(event() | legacy_event()) -> event().
maybe_migrate(Ev) ->
    Ev.
