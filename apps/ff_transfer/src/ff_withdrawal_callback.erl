-module(ff_withdrawal_callback).

-define(ACTUAL_FORMAT_VERSION, 1).

%% Types

-type tag() :: binary().
-type payload() :: binary().

-type params() :: #{
    tag := tag()
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
    pending |
    succeeded.

%%

-type event() ::
    {created, callback()} |
    {finished, response()} |
    {status_changed, status()}.

-type process_result() :: [event()].

%%

-export_type([
    tag/0,
    payload/0,
    params/0,
    callback/0,
    response/0,
    status/0
]).

-export([
    tag/1,
    status/1,
    response/1,
    create/1
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

-spec create(params()) ->
    {ok, process_result()}.

create(#{tag := Tag}) ->
    Callback = #{
        version => ?ACTUAL_FORMAT_VERSION,
        tag => Tag
    },
    {ok, [{created, Callback}, {status_changed, pending}]}.