%%%
%%% w2w transfer machine
%%%

-module(w2w_transfer_machine).

-behaviour(machinery).

%% API

-type id() :: machinery:id().
-type change() :: w2w_transfer:event().
-type event() :: {integer(), ff_machine:timestamped_event(change())}.
-type st() :: ff_machine:st(w2w_transfer()).
-type w2w_transfer() :: w2w_transfer:w2w_transfer().
-type external_id() :: id().
-type event_range() :: {After :: non_neg_integer() | undefined, Limit :: non_neg_integer() | undefined}.

-type params() :: w2w_transfer:params().
-type create_error() ::
    w2w_transfer:create_error() |
    exists.

-type start_adjustment_error() ::
    w2w_transfer:start_adjustment_error() |
    unknown_w2w_transfer_error().

-type unknown_w2w_transfer_error() ::
    {unknown_w2w_transfer, id()}.

-type repair_error() :: ff_repair:repair_error().
-type repair_response() :: ff_repair:repair_response().

-export_type([id/0]).
-export_type([st/0]).
-export_type([change/0]).
-export_type([event/0]).
-export_type([params/0]).
-export_type([w2w_transfer/0]).
-export_type([event_range/0]).
-export_type([external_id/0]).
-export_type([create_error/0]).
-export_type([start_adjustment_error/0]).
-export_type([repair_error/0]).
-export_type([repair_response/0]).

%% API

-export([create/2]).
-export([get/1]).
-export([get/2]).
-export([events/2]).
-export([repair/2]).

-export([start_adjustment/2]).

%% Accessors

-export([w2w_transfer/1]).
-export([ctx/1]).

%% Machinery

-export([init/4]).
-export([process_timeout/3]).
-export([process_repair/4]).
-export([process_call/4]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%% Internal types

-type ctx()           :: ff_entity_context:context().
-type adjustment_params()        :: w2w_transfer:adjustment_params().

-type call() ::
    {start_adjustment, adjustment_params()}.

-define(NS, 'ff/w2w_transfer_v1').

%% API

-spec create(params(), ctx()) ->
    ok |
    {error, w2w_transfer:create_error() | exists}.

create(Params, Ctx) ->
    do(fun () ->
        #{id := ID} = Params,
        Events = unwrap(w2w_transfer:create(Params)),
        unwrap(machinery:start(?NS, ID, {Events, Ctx}, backend()))
    end).

-spec get(id()) ->
    {ok, st()} |
    {error, unknown_w2w_transfer_error()}.

get(ID) ->
    get(ID, {undefined, undefined}).

-spec get(id(), event_range()) ->
    {ok, st()} |
    {error, unknown_w2w_transfer_error()}.

get(ID, {After, Limit}) ->
    case ff_machine:get(w2w_transfer, ?NS, ID, {After, Limit, forward}) of
        {ok, _Machine} = Result ->
            Result;
        {error, notfound} ->
            {error, {unknown_w2w_transfer, ID}}
    end.

-spec events(id(), event_range()) ->
    {ok, [event()]} |
    {error, unknown_w2w_transfer_error()}.

events(ID, {After, Limit}) ->
    case machinery:get(?NS, ID, {After, Limit, forward}, backend()) of
        {ok, #{history := History}} ->
            {ok, [{EventID, TsEv} || {EventID, _, TsEv} <- History]};
        {error, notfound} ->
            {error, {unknown_w2w_transfer, ID}}
    end.

-spec repair(id(), ff_repair:scenario()) ->
    {ok, repair_response()} | {error, notfound | working | {failed, repair_error()}}.
repair(ID, Scenario) ->
    machinery:repair(?NS, ID, Scenario, backend()).

-spec start_adjustment(id(), adjustment_params()) ->
    ok |
    {error, start_adjustment_error()}.

start_adjustment(W2WTransferID, Params) ->
    call(W2WTransferID, {start_adjustment, Params}).

%% Accessors

-spec w2w_transfer(st()) ->
    w2w_transfer().

w2w_transfer(St) ->
    ff_machine:model(St).

-spec ctx(st()) ->
    ctx().

ctx(St) ->
    ff_machine:ctx(St).

%% Machinery

-type machine()      :: ff_machine:machine(event()).
-type result()       :: ff_machine:result(event()).
-type handler_opts() :: machinery:handler_opts(_).
-type handler_args() :: machinery:handler_args(_).

-spec init({[event()], ctx()}, machine(), handler_args(), handler_opts()) ->
    result().

init({Events, Ctx}, #{}, _, _Opts) ->
    #{
        events    => ff_machine:emit_events(Events),
        action    => continue,
        aux_state => #{ctx => Ctx}
    }.

-spec process_timeout(machine(), handler_args(), handler_opts()) ->
    result().

process_timeout(Machine, _, _Opts) ->
    St = ff_machine:collapse(w2w_transfer, Machine),
    W2WTransfer = w2w_transfer(St),
    process_result(w2w_transfer:process_transfer(W2WTransfer)).

-spec process_call(call(), machine(), handler_args(), handler_opts()) -> {Response, result()} when
    Response :: ok | {error, w2w_transfer:start_adjustment_error()}.

process_call({start_adjustment, Params}, Machine, _, _Opts) ->
    do_start_adjustment(Params, Machine);
process_call(CallArgs, _Machine, _, _Opts) ->
    erlang:error({unexpected_call, CallArgs}).

-spec process_repair(ff_repair:scenario(), machine(), handler_args(), handler_opts()) ->
    {ok, {repair_response(), result()}} | {error, repair_error()}.

process_repair(Scenario, Machine, _Args, _Opts) ->
    ff_repair:apply_scenario(w2w_transfer, Machine, Scenario).

%% Internals

backend() ->
    fistful:backend(?NS).

-spec do_start_adjustment(adjustment_params(), machine()) -> {Response, result()} when
    Response :: ok | {error, w2w_transfer:start_adjustment_error()}.

do_start_adjustment(Params, Machine) ->
    St = ff_machine:collapse(w2w_transfer, Machine),
    case w2w_transfer:start_adjustment(Params, w2w_transfer(St)) of
        {ok, Result} ->
            {ok, process_result(Result)};
        {error, _Reason} = Error ->
            {Error, #{}}
    end.

process_result({Action, Events}) ->
    genlib_map:compact(#{
        events => set_events(Events),
        action => Action
    }).

set_events([]) ->
    undefined;
set_events(Events) ->
    ff_machine:emit_events(Events).

call(ID, Call) ->
    case machinery:call(?NS, ID, Call, backend()) of
        {ok, Reply} ->
            Reply;
        {error, notfound} ->
            {error, {unknown_w2w_transfer, ID}}
    end.