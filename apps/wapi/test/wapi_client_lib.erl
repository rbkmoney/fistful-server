-module(wapi_client_lib).

-export([get_context/4]).
-export([get_context/5]).
-export([get_context/6]).

-export([handle_response/1]).
-export([make_request/2]).

-export([make_search_query_string/1]).
-export([default_event_handler/0]).

-type context() :: #{
    url := string(),
    token := term(),
    timeout := integer(),
    event_handler := event_handler(),
    protocol := protocol(),
    deadline := iolist() | undefined
}.

-export_type([context/0]).

-type event_handler() :: fun((event_type(), code(), duration()) -> ok).

-export_type([event_handler/0]).

-type event_type() :: atom().
-type code() :: pos_integer().
-type duration() :: non_neg_integer().

-type search_query() :: list().

-export_type([search_query/0]).

-type query_string() :: map().

-export_type([query_string/0]).

-type header() :: {binary(), binary()}.

-export_type([header/0]).

-type protocol() :: ipv4 | ipv6.

-export_type([protocol/0]).

-type protocol_opts() :: [{connect_options, [inet4 | inet6]}].

-export_type([protocol_opts/0]).

-spec protocol_to_opt(protocol()) -> protocol_opts().
protocol_to_opt(ipv4) ->
    [{connect_options, [inet]}];
protocol_to_opt(ipv6) ->
    [{connect_options, [inet6]}].

-spec make_search_query_string(search_query()) -> query_string().
make_search_query_string(ParamList) ->
    lists:foldl(fun(Elem, Acc) -> maps:merge(Acc, prepare_param(Elem)) end, #{}, ParamList).

-spec prepare_param({atom(), term()}) -> map().
prepare_param(Param) ->
    case Param of
        {limit, P} -> #{<<"limit">> => genlib:to_binary(P)};
        {offset, P} -> #{<<"offset">> => genlib:to_binary(P)};
        {from_time, P} -> #{<<"fromTime">> => genlib_format:format_datetime_iso8601(P)};
        {to_time, P} -> #{<<"toTime">> => genlib_format:format_datetime_iso8601(P)};
        {status, P} -> #{<<"status">> => genlib:to_binary(P)};
        {split_unit, P} -> #{<<"splitUnit">> => genlib:to_binary(P)};
        {split_size, P} -> #{<<"splitSize">> => genlib:to_binary(P)};
        {payment_method, P} -> #{<<"paymentMethod">> => genlib:to_binary(P)};
        {ParamName, P} -> #{genlib:to_binary(ParamName) => P}
    end.

-spec make_request(context(), map()) -> {string(), map(), list()}.
make_request(Context, ParamsList) ->
    {Url, Headers} = get_http_params(Context),
    Opts = get_hackney_opts(Context),
    PreparedParams = make_params(Headers, ParamsList),
    {Url, PreparedParams, Opts}.

-spec make_params(list(), map()) -> map().
make_params(Headers, RequestParams) ->
    Params = #{
        header => maps:from_list(Headers),
        binding => #{},
        body => #{},
        qs_val => #{}
    },
    maps:merge(Params, RequestParams).

-spec handle_response({atom(), Code :: integer(), RespHeaders :: list(), Body :: term()}) ->
    {ok, term()} | {error, term()}.
handle_response(Response) ->
    case Response of
        {ok, Code, Headers, Body} -> handle_response(Code, Headers, Body);
        {error, Error} -> {error, Error}
    end.

-spec handle_response(integer(), list(), term()) -> {ok, term()} | {error, term()}.
handle_response(Code, _, _) when Code =:= 204 ->
    {ok, undefined};
handle_response(303, Headers, _) ->
    URL = proplists:get_value(<<"Location">>, Headers),
    {ok, {redirect, URL}};
handle_response(Code, _, Body) when Code div 100 == 2 ->
    %% 2xx HTTP code
    {ok, decode_body(Body)};
handle_response(Code, _, Body) ->
    {error, {Code, Body}}.

-spec get_context(string(), term(), integer(), protocol()) -> context().
get_context(Url, Token, Timeout, Protocol) ->
    get_context(Url, Token, Timeout, Protocol, default_event_handler()).

-spec get_context(string(), term(), integer(), protocol(), event_handler()) -> context().
get_context(Url, Token, Timeout, Protocol, EventHandler) ->
    get_context(Url, Token, Timeout, Protocol, EventHandler, undefined).

-spec get_context(string(), term(), integer(), protocol(), event_handler(), iolist() | undefined) -> context().
get_context(Url, Token, Timeout, Protocol, EventHandler, Deadline) ->
    #{
        url => Url,
        token => Token,
        timeout => Timeout,
        protocol => Protocol,
        event_handler => EventHandler,
        deadline => Deadline
    }.

-spec default_event_handler() -> event_handler().
default_event_handler() ->
    fun(_Type, _Code, _Duration) ->
        ok
    end.

-spec get_http_params(context()) -> {string(), list()}.
get_http_params(Context) ->
    Url = maps:get(url, Context),
    Headers = headers(Context),
    {Url, Headers}.

-spec get_hackney_opts(context()) -> list().
get_hackney_opts(Context) ->
    protocol_to_opt(maps:get(protocol, Context, ipv4)) ++
        [
            {connect_timeout, maps:get(timeout, Context, 5000)},
            {recv_timeout, maps:get(timeout, Context, 5000)}
        ].

-spec headers(context()) -> list(header()).
headers(#{deadline := Deadline} = Context) ->
    RequiredHeaders = x_request_deadline_header(Deadline, [x_request_id_header() | json_accept_headers()]),
    case maps:get(token, Context) of
        <<>> ->
            RequiredHeaders;
        Token ->
            [auth_header(Token) | RequiredHeaders]
    end.

-spec x_request_id_header() -> header().
x_request_id_header() ->
    {<<"X-Request-ID">>, integer_to_binary(rand:uniform(100000))}.

-spec x_request_deadline_header(iolist() | undefined, list()) -> list().
x_request_deadline_header(undefined, Headers) ->
    Headers;
x_request_deadline_header(Time, Headers) ->
    [{<<"X-Request-Deadline">>, Time} | Headers].

-spec auth_header(term()) -> header().
auth_header(Token) ->
    {<<"Authorization">>, <<"Bearer ", Token/binary>>}.

-spec json_accept_headers() -> list(header()).
json_accept_headers() ->
    [
        {<<"Accept">>, <<"application/json">>},
        {<<"Accept-Charset">>, <<"UTF-8">>},
        {<<"Content-Type">>, <<"application/json; charset=UTF-8">>}
    ].

-spec decode_body(term()) -> term().
decode_body(Body) when is_binary(Body) ->
    jsx:decode(Body, [return_maps]);
decode_body(Body) ->
    Body.
