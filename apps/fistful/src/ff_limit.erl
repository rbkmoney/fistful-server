%%%
%%% Limit tracker
%%%
%%% Behaviour:
%%%
%%%  - If _limit_ is exceeded then there's no need to reject the transaction.
%%%
%%%  - _Account_ operation is idempotent as long as the transaction is nor
%%%    confirmed neither rejected.
%%%
%%%    After that any transaction w/ the same ID will be handled regularly as a
%%%    distinct transaction.
%%%
%%%  - Limit itself is _not_ part of the state, just the _timespan_, implicitly.
%%%
%%%    In a nutshell, we derive underlying 'account ID' from the timespan for
%%%    the sake of simplicity. There are side effect though:
%%%     * limits are independent in a sense that, for example, _daily_ limit
%%%       changes do not count towards _monthly_ limit, and
%%%     * there is no way to know that two transactions are one and the same if
%%%       their IDs are equal but their timestamps are too far apart.
%%%
%%%  - Accounting does not respect timezone-related quirks.
%%%
%%%    If you want to, you should do it yourself. For example, you could convert
%%%    UTC timestamps to timezone-specific timestamps and feed them here.
%%%
%%%    For some reason which I can not wrap my head around `localtime` can
%%%    resolve one UTC timestamp to _two_ timezone-specific timestamps in the
%%%    middle of DST transition and let us resolve ambiguity. I believe taking
%%%    earliest one would do the trick.
%%%

-module(ff_limit).

%% API

-export([account/4]).
-export([confirm/4]).
-export([reject/4]).

-export([get/4]).

%% Machinery

-behaviour(machinery).

-export([init/4]).
-export([process_timeout/3]).
-export([process_call/4]).

%% Types

-type limit(T)    :: {id(), range(T), timespan()}.
-type range(T)    :: ff_range:range(T).
-type timespan()  :: day | week | month | year.
-type trxid()     :: binary().
-type delta(T)    :: ord(T).
-type trx(T)      :: {trxid(), timestamp(), delta(T)}.
-type record(T)   :: ff_indef:indef(T).

-type timestamp() :: machinery:timestamp().
-type ord(T)      :: T. % totally ordered

%% API

-type namespace()     :: machinery:namespace().
-type id()            :: machinery:id().
-type backend()       :: machinery:backend(_).

-spec account(namespace(), limit(T), trx(T), backend()) ->
    {ok, record(T)}                |
    {error, {exceeded, record(T)}} |
    {error, {conflict, trx(T)}}    .

-spec confirm(namespace(), limit(T), trx(T), backend()) ->
    {ok, record(T)}                |
    {error, {conflict, trx(T)}}    .

-spec reject(namespace(), limit(T), trx(T), backend()) ->
    {ok, record(T)}                |
    {error, {conflict, trx(T)}}    .

-spec get(namespace(), limit(T), timestamp(), backend()) ->
    {ok, record(T)} | {error, notfound}.

account(NS, Limit, Trx, Backend) ->
    ID    = construct_limit_machine_id(Limit, Trx),
    Range = get_limit_range(Limit),
    lazycall(NS, ID, {account, Trx, Range}, Backend).

confirm(NS, Limit, Trx, Backend) ->
    ID = construct_limit_machine_id(Limit, Trx),
    lazycall(NS, ID, {confirm, Trx}, Backend).

reject(NS, Limit, Trx, Backend) ->
    ID = construct_limit_machine_id(Limit, Trx),
    lazycall(NS, ID, {reject, Trx}, Backend).

get(NS, Limit, Ts, Backend) ->
    ID = construct_limit_machine_id_(Limit, Ts),
    case machinery:get(NS, ID, {undefined, 0, forward}, Backend) of
        {ok, #{aux_state := St}} ->
            {ok, head(St)};
        {error, notfound} ->
            {error, notfound}
    end.

lazycall(NS, ID, Call, Backend) ->
    case machinery:call(NS, ID, {undefined, 0, forward}, Call, Backend) of
        {ok, Response} ->
            Response;
        {error, notfound} ->
            _ = machinery:start(NS, ID, 0, Backend),
            lazycall(NS, ID, Call, Backend)
    end.

construct_limit_machine_id(Limit, Trx) ->
    construct_limit_machine_id_(Limit, get_trx_ts(Trx)).

construct_limit_machine_id_(Limit, Ts) ->
    ID     = get_limit_id(Limit),
    Span   = get_limit_span(Limit),
    Bucket = find_bucket(Ts, Span),
    ff_string:join($/, [
        limit,
        ID,
        Span,
        Bucket
    ]).

find_bucket({{Date, _Time}, _USec}, Span) ->
    find_bucket(Date, Span);

find_bucket(Date, day) ->
    calendar:date_to_gregorian_days(Date);
find_bucket(Date, week) ->
    {Y, W} = calendar:iso_week_number(Date),
    Y * 100 + W;
find_bucket({Y, M, _}, month) ->
    Y * 100 + M;
find_bucket({Y, _, _}, year) ->
    Y.

%% Machinery

-type ev(T) ::
    {seed    , ord(T)} |
    {account , trx(T)} |
    {confirm , trx(T)} |
    {reject  , trx(T)} .

-type machine(T)      :: machinery:machine(ev(T)).
-type result(T)       :: machinery:result(ev(T)).
-type handler_opts()  :: machinery:handler_opts().

-spec init(ord(T), machine(T), _, handler_opts()) ->
    result(T).

-spec process_timeout(machine(T), _, handler_opts()) ->
    result(T).

-type call(T) ::
    {account , trx(T), limit(T)} |
    {confirm , trx(T)}           |
    {reject  , trx(T)}           .

-spec process_call(call(T), machine(T), _, handler_opts()) ->
    {
        {ok, record(T)}             |
        {error, {conflict, ord(T)}} ,
        result(T)
    }.

init(Seed, #{}, _, _Opts) ->
    #{
        events    => [{seed, Seed}],
        aux_state => new_st(Seed)
    }.

process_timeout(#{}, _, _Opts) ->
    #{}.

process_call({account, Trx, Limit}, #{aux_state := St}, _, _Opts) ->
    process_account(Trx, Limit, St);
process_call({confirm, Trx}, #{aux_state := St}, _, _Opts) ->
    process_confirm(Trx, St);
process_call({reject, Trx}, #{aux_state := St}, _, _Opts) ->
    process_reject(Trx, St).

process_account(Trx, Range, St0) ->
    case lookup_trx(get_trx_id(Trx), St0) of
        error ->
            St1 = record_trx(Trx, St0),
            Head1 = head(St1),
            case ff_range:contains(Range, ff_indef:to_range(Head1)) of
                true ->
                    {{ok, Head1}, #{
                        events    => [{account, Trx}],
                        aux_state => St1
                    }};
                false ->
                    {{error, {exceeded, Head1}}, #{}}
            end;
        {ok, Trx} ->
            {{ok, head(St0)}, #{}};
        {ok, TrxWas} ->
            {{error, {conflict, TrxWas}}, #{}}
    end.

process_confirm(Trx, St0) ->
    case lookup_trx(get_trx_id(Trx), St0) of
        {ok, Trx} ->
            St1 = confirm_trx(Trx, St0),
            {{ok, head(St1)}, #{
                events    => [{confirm, Trx}],
                aux_state => St1
            }};
        {ok, TrxWas} ->
            {{error, {conflict, TrxWas}}, #{}};
        error ->
            {{ok, head(St0)}, #{}}
    end.

process_reject(Trx, St0) ->
    case lookup_trx(get_trx_id(Trx), St0) of
        {ok, Trx} ->
            St1 = reject_trx(Trx, St0),
            {{ok, head(St1)}, #{
                events    => [{reject, Trx}],
                aux_state => St1
            }};
        {ok, TrxWas} ->
            {{error, {conflict, TrxWas}}, #{}};
        error ->
            {{ok, head(St0)}, #{}}
    end.

%%

new_st(Seed) ->
    #{
        head => ff_indef:new(Seed),
        trxs => #{}
    }.

head(#{head := Head}) ->
    Head.

lookup_trx(TrxID, #{trxs := Trxs}) ->
    maps:find(TrxID, Trxs).

record_trx(Trx, St = #{head := Head, trxs := Trxs}) ->
    St#{
        head := ff_indef:account(get_trx_dv(Trx), Head),
        trxs := maps:put(get_trx_id(Trx), Trx, Trxs)
    }.

confirm_trx(Trx, St = #{head := Head, trxs := Trxs}) ->
    St#{
        head := ff_indef:confirm(get_trx_dv(Trx), Head),
        trxs := maps:remove(get_trx_id(Trx), Trxs)
    }.

reject_trx(Trx, St = #{head := Head, trxs := Trxs}) ->
    St#{
        head := ff_indef:reject(get_trx_dv(Trx), Head),
        trxs := maps:remove(get_trx_id(Trx), Trxs)
    }.

%%

get_trx_id({ID, _Ts, _Dv}) ->
    ID.
get_trx_ts({_ID, Ts, _Dv}) ->
    Ts.
get_trx_dv({_ID, _Ts, Dv}) ->
    Dv.

get_limit_id({ID, _Range, _Span}) ->
    ID.
get_limit_range({_ID, Range, _Span}) ->
    Range.
get_limit_span({_ID, _Range, Span}) ->
    Span.
