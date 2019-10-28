-module(p2p_callback).

-define(ACTUAL_FORMAT_VERSION, 1).

-type tag() :: binary().
-type payload() :: binary().

-opaque p2p_callback() :: #{
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

-type status() ::
    pending |
    succeeded.

-type processed_callback() :: #{
    payload => payload(),
    response => response()
}.

-type legacy_event() :: any().
-type event() ::
    {created, p2p_callback()} |
    {succeeded, processed_callback()} |
    {status_changed, status()}.

-export_type([tag/0]).
-export_type([event/0]).
-export_type([response/0]).
-export_type([status/0]).
-export_type([p2p_callback/0]).
-export_type([params/0]).

%% Accessors

-export([tag/1]).
-export([status/1]).
-export([response/1]).

%% API

-export([create/1]).
-export([is_active/1]).
-export([is_finished/1]).
-export([process_callback/1]).

%% Event source

-export([apply_event/2]).
-export([maybe_migrate/1]).

%% Internal types

-type action() :: machinery:action() | undefined.
-type process_result() :: {action(), [event()]}.

%% Accessors

-spec tag(p2p_callback()) -> tag().
tag(#{tag := V}) ->
    V.

-spec status(p2p_callback()) -> status().
status(#{status := V}) ->
    V.

-spec response(p2p_callback()) -> response() | undefined.
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
    {ok, {continue, [{created, Callback}, {status_changed, pending}]}}.

%% Сущность в настоящий момент нуждается в передаче ей управления для совершения каких-то действий
-spec is_active(p2p_callback()) -> boolean().
is_active(#{status := succeeded}) ->
    false;
is_active(#{status := pending}) ->
    true.

%% Сущность приняла статус, который не будет меняться без внешних воздействий.
-spec is_finished(p2p_callback()) -> boolean().
is_finished(#{status := succeeded}) ->
    true;
is_finished(#{status := pending}) ->
    false.

-spec process_callback(p2p_callback()) ->
    {response(), process_result()}.
process_callback(Callback) ->
    case status(Callback) of
        pending ->
            % TODO add here p2p adapter call
            Response = #{payload => <<"Test payload">>},
            {Response, {undefined, [
                {succeeded, #{payload => <<"Test payload">>, response => <<"Test response">>}},
                {status_changed, succeeded}
            ]}};
        succeeded ->
            Response = response(Callback),
            {Response, {undefined, []}}
    end.

%% Internals

-spec update_status(status(), p2p_callback()) -> p2p_callback().
update_status(Status, Callback) ->
    Callback#{status := Status}.

%% Events utils

-spec apply_event(event() | legacy_event(), p2p_callback() | undefined) ->
    p2p_callback().
apply_event(Ev, T) ->
    apply_event_(maybe_migrate(Ev), T).

-spec apply_event_(event(), p2p_callback() | undefined) ->
    p2p_callback().
apply_event_({created, T}, undefined) ->
    T;
apply_event_({status_changed, S}, T) ->
    update_status(S, T).

-spec maybe_migrate(event() | legacy_event()) ->
    event().
maybe_migrate(Ev) ->
    Ev.
