%%%
%%% P2P template machine
%%%

-module(p2p_template_machine).
-behaviour(machinery).

-define(NS, 'ff/p2p_template_v1').

%% API

-export([template/1]).

-export([create/2]).
-export([get/1]).
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
-type id() :: machinery:id().
-type params() :: p2p_template:params().

-type machine() :: ff_machine:machine(event()).
-type result() :: ff_machine:result(event()).
-type handler_opts() :: machinery:handler_opts(_).
-type handler_args() :: machinery:handler_args(_).

-type st() :: ff_machine:st(template()).
-type template() :: p2p_template:template().
-type event() :: p2p_template:event().
-type event_id() :: integer().
-type events() :: [{event_id(), ff_machine:timestamped_event(event())}].
-type ctx() :: ff_entity_context:context().

-export_type([events/0]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%
%% API
%%

-spec get(ref()) ->
    {ok, st()}        |
    {error, unknown_p2p_template_error()}.

get(Ref) ->
    case ff_machine:get(p2p_template, ?NS, Ref) of
        {ok, _Machine} = Result ->
            Result;
        {error, notfound} ->
            {error, {unknown_p2p_template, Ref}}
    end.

-spec template(st()) -> template().

template(St) ->
    ff_machine:model(St).

%%

-spec create(params(), ctx()) ->
    ok | {error, exists}.
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
    ok | {error, notfound | working}.
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
    no_return().

process_call(CallArgs, _Machine, _, _Opts) ->
    erlang:error({unexpected_call, CallArgs}).

-spec process_repair(ff_repair:scenario(), machine(), handler_args(), handler_opts()) ->
    result().
process_repair(Scenario, Machine, _Args, _Opts) ->
    ScenarioProcessors = #{},
    ff_repair:apply_scenario(p2p_template, Machine, Scenario, ScenarioProcessors).

%%
%% Internals
%%

backend() ->
    fistful:backend(?NS).
