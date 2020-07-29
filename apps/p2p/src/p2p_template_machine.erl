%%%
%%% P2P template machine
%%%

-module(p2p_template_machine).
-behaviour(machinery).

-define(NS, 'ff/p2p_template_v1').

%% API

-export([p2p_template/1]).
-export([create_transfer/2]).
-export([get_quote/2]).

-export([set_blocking/2]).
-export([create/2]).
-export([get/1]).
-export([get/2]).
-export([events/2]).
-export([repair/2]).

%% machinery

-export([init/4]).
-export([process_timeout/3]).
-export([process_repair/4]).
-export([process_call/4]).

%%
%% Types
%%

-type unknown_p2p_template_error() ::
    {unknown_p2p_template, ref()}.

%%
%% Internal types
%%

-type ref() :: machinery:ref().
-type range() :: machinery:range().
-type id() :: machinery:id().
-type repair_error() :: ff_repair:repair_error().
-type repair_response() :: ff_repair:repair_response().
-type params() :: p2p_template:params().

-type machine() :: ff_machine:machine(event()).
-type result() :: ff_machine:result(event()).
-type handler_opts() :: machinery:handler_opts(_).
-type handler_args() :: machinery:handler_args(_).

-type template() :: p2p_template:template_state().
-type st() :: ff_machine:st(template()).
-type event() :: p2p_template:event().
-type event_id() :: integer().
-type events() :: [{event_id(), ff_machine:timestamped_event(event())}].
-type ctx() :: ff_entity_context:context().
-type blocking() :: p2p_template:blocking().
-type create_error() :: p2p_template:create_error().

-export_type([events/0]).
-export_type([params/0]).
-export_type([unknown_p2p_template_error/0]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%
%% API
%%

-spec get(ref()) ->
    {ok, st()}        |
    {error, unknown_p2p_template_error()}.

get(Ref) ->
    get(Ref, {undefined, undefined, forward}).

-spec get(ref(), range()) ->
    {ok, st()}        |
    {error, unknown_p2p_template_error()}.

get(Ref, Range) ->
    case ff_machine:get(p2p_template, ?NS, Ref, Range) of
        {ok, _Machine} = Result ->
            Result;
        {error, notfound} ->
            {error, {unknown_p2p_template, Ref}}
    end.

-spec p2p_template(st()) -> template().

p2p_template(St) ->
    ff_machine:model(St).

-spec get_quote(id(), p2p_template:quote_params()) ->
    {ok, p2p_quote:quote()} |
    {error, p2p_quote:get_quote_error() | unknown_p2p_template_error()}.
get_quote(ID, #{
    body := Body,
    sender := Sender,
    receiver := Receiver
}) ->
    do(fun() ->
        Machine = unwrap(p2p_template_machine:get(ID)),
        State = p2p_template(Machine),
        unwrap(p2p_quote:get_quote(Body, p2p_template:identity_id(State), Sender, Receiver))
    end).

-spec create_transfer(id(), p2p_template:transfer_params()) ->
    ok | {error, p2p_transfer:create_error() | exists | unknown_p2p_template_error()}.
create_transfer(ID, Params) ->
    do(fun() ->
        Machine = unwrap(p2p_template_machine:get(ID)),
        State = p2p_template(Machine),
        unwrap(p2p_template:create_transfer(Params, State))
    end).

%%

-spec set_blocking(id(), blocking()) ->
    ok | {error, unknown_p2p_template_error()}.
set_blocking(ID, Blocking) ->
    call(ID, {set_blocking, Blocking}).

-spec create(params(), ctx()) ->
    ok | {error, exists | create_error()}.
create(Params = #{id := ID}, Ctx) ->
    do(fun () ->
        Events = unwrap(p2p_template:create(Params)),
        unwrap(machinery:start(?NS, ID, {Events, Ctx}, backend()))
    end).

-spec events(id(), machinery:range()) ->
    {ok, events()} |
    {error, unknown_p2p_template_error()}.

events(Ref, Range) ->
    case ff_machine:history(p2p_template, ?NS, Ref, Range) of
        {ok, History} ->
            Events = [{EventID, TsEv} || {EventID, _, TsEv} <- History],
            {ok, Events};
        {error, notfound} ->
            {error, {unknown_p2p_template, Ref}}
    end.

-spec repair(ref(), ff_repair:scenario()) ->
    {ok, repair_response()} | {error, notfound | working | {failed, repair_error()}}.
repair(Ref, Scenario) ->
    machinery:repair(?NS, Ref, Scenario, backend()).

%% machinery callbacks

-spec init({[event()], ctx()}, machine(), handler_args(), handler_opts()) ->
    result().
init({Events, Ctx}, #{}, _, _Opts) ->
    #{
        events    => ff_machine:emit_events(Events),
        aux_state => #{ctx => Ctx}
    }.

-spec process_timeout(machine(), handler_args(), handler_opts()) ->
    no_return().
process_timeout(Machine, _, _Opts) ->
    erlang:error({unexpected_timeout, Machine}).

-spec process_call(any(), machine(), handler_args(), handler_opts()) ->
    {Response, result()} | no_return() when
    Response :: ok.
process_call({set_blocking, Blocking}, Machine, _, _Opts) ->
    do_set_blocking(Blocking, Machine);
process_call(CallArgs, _Machine, _, _Opts) ->
    erlang:error({unexpected_call, CallArgs}).

-spec process_repair(ff_repair:scenario(), machine(), handler_args(), handler_opts()) ->
    {ok, {repair_response(), result()}} | {error, repair_error()}.
process_repair(Scenario, Machine, _Args, _Opts) ->
    ff_repair:apply_scenario(p2p_template, Machine, Scenario).

%%
%% Internals
%%

do_set_blocking(Blocking, Machine) ->
    St = ff_machine:collapse(p2p_template, Machine),
    {ok, Result} = p2p_template:set_blocking(Blocking, p2p_template(St)),
    {ok, process_result(Result, St)}.

process_result({Action, Events}, _St) ->
    genlib_map:compact(#{
        events => set_events(Events),
        action => Action
    }).

set_events(Events) ->
    ff_machine:emit_events(Events).

call(ID, Call) ->
    case machinery:call(?NS, ID, Call, backend()) of
        {ok, Reply} ->
            Reply;
        {error, notfound} ->
            {error, {unknown_p2p_template, ID}}
    end.

backend() ->
    fistful:backend(?NS).
