%%%
%%% Sequential ID generator
%%%

-module(ff_sequence).

-behaviour(machinery).

%% API

-type sequence() :: non_neg_integer().

-export([next/3]).
-export([get/3]).

-export_type([sequence/0]).

%% Machinery

-export([init/4]).
-export([process_timeout/3]).
-export([process_repair/4]).
-export([process_call/4]).

%%

%% API

-type namespace()     :: machinery:namespace().
-type id()            :: machinery:id().

-spec next(namespace(), id(), machinery:backend(_)) ->
    sequence().

-spec get(namespace(), id(), machinery:backend(_)) ->
    sequence().

next(NS, ID, Be) ->
    case machinery:call(NS, ID, {undefined, 0, forward}, {increment, 1}, Be) of
        {ok, Seq} ->
            Seq;
        {error, notfound} ->
            _ = machinery:start(NS, ID, 0, Be),
            next(NS, ID, Be)
    end.

get(NS, ID, Be) ->
    case machinery:get(NS, ID, {undefined, 0, forward}, Be) of
        {ok, #{aux_state := Seq}} ->
            Seq;
        {error, notfound} ->
            0
    end.

%% Machinery

-type increment()    :: pos_integer().

-type ev() ::
    {increment, increment()}.

-type auxst() ::
    sequence().

-type machine()      :: machinery:machine(ev(), auxst()).
-type result()       :: machinery:result(ev(), auxst()).
-type handler_opts() :: machinery:handler_opts(_).

-spec init(increment(), machine(), _, handler_opts()) ->
    result().

-spec process_timeout(machine(), _, handler_opts()) ->
    result().

-type call() ::
    {increment, increment()}.

-spec process_call(call(), machine(), _, handler_opts()) ->
    {sequence(), result()}.

init(Inc, #{}, _, _Opts) ->
    #{
        events    => [{increment, Inc}],
        aux_state => Inc
    }.

process_timeout(#{}, _, _Opts) ->
    #{}.

process_call({increment, Inc}, #{aux_state := Seq0}, _, _Opts) ->
    Seq1 = Seq0 + Inc,
    {Seq1, #{
        events    => [{increment, Inc}],
        aux_state => Seq1
    }}.

-spec process_repair(ff_repair:scenario(), machine(), machinery:handler_args(_), machinery:handler_opts(_)) ->
    no_return().

process_repair(_RepairArgs, _Machine, _Args, _Opts) ->
    erlang:error({not_implemented, repair}).
