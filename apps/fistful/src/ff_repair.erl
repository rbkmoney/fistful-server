-module(ff_repair).

-export([apply_scenario/3]).
-export([apply_scenario/4]).

%% Types

-type scenario() ::
    scenario_id()
    | {scenario_id(), scenario_args()}.

-type scenario_id() :: atom().
-type scenario_args() :: any().
-type scenario_result(Event, AuxState) :: machinery:result(Event, AuxState).
-type scenario_result() :: scenario_result(model_event(), model_aux_state()).
-type scenario_error() :: machinery:error(any()).
-type scenario_response() :: machinery:response(any()).

-type processor() :: fun(
    (scenario_args(), machine()) -> {ok, {scenario_response(), scenario_result()}} | {error, scenario_error()}
).

-type processors() :: #{
    scenario_id() := processor()
}.

-type repair_error() ::
    unknown_scenario_error()
    | invalid_result_error()
    | scenario_error().

-type repair_response() ::
    ok
    | scenario_response().

-type invalid_result_error() ::
    {invalid_result, unexpected_failure}.

-type unknown_scenario_error() ::
    {unknown_scenario, {scenario_id(), [scenario_id()]}}.

-export_type([scenario/0]).
-export_type([scenario_id/0]).
-export_type([scenario_args/0]).
-export_type([scenario_result/0]).
-export_type([scenario_result/2]).
-export_type([scenario_error/0]).
-export_type([processor/0]).
-export_type([processors/0]).
-export_type([repair_error/0]).
-export_type([repair_response/0]).
-export_type([invalid_result_error/0]).

%% Internal types

-type model_event() :: any().
-type model_aux_state() :: any().
-type event() :: ff_machine:timestamped_event(model_event()).
-type result() :: machinery:result(event(), model_aux_state()).
-type machine() :: ff_machine:machine(event()).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%% API

-spec apply_scenario(module(), machine(), scenario()) -> {ok, {repair_response(), result()}} | {error, repair_error()}.
apply_scenario(Mod, Machine, Scenario) ->
    apply_scenario(Mod, Machine, Scenario, #{}).

-spec apply_scenario(module(), machine(), scenario(), processors()) ->
    {ok, {repair_response(), result()}} | {error, repair_error()}.
apply_scenario(Mod, Machine, Scenario, ScenarioProcessors) ->
    {ScenarioID, ScenarioArgs} = unwrap_scenario(Scenario),
    AllProcessors = add_default_processors(ScenarioProcessors),
    do(fun() ->
        Processor = unwrap(get_processor(ScenarioID, AllProcessors)),
        {Response, Result} = unwrap(apply_processor(Processor, ScenarioArgs, Machine)),
        valid = unwrap(validate_result(Mod, Machine, Result)),
        {Response, Result}
    end).

%% Internals

-spec get_processor(scenario_id(), processors()) -> {ok, processor()} | {error, unknown_scenario_error()}.
get_processor(ScenarioID, Processors) ->
    case maps:find(ScenarioID, Processors) of
        {ok, _Processor} = Result ->
            Result;
        error ->
            {unknown_scenario, {ScenarioID, maps:keys(Processors)}}
    end.

-spec unwrap_scenario(scenario()) -> {scenario_id(), scenario_args()}.
unwrap_scenario(ScenarioID) when is_atom(ScenarioID) ->
    {ScenarioID, undefined};
unwrap_scenario({ScenarioID, ScenarioArgs}) when is_atom(ScenarioID) ->
    {ScenarioID, ScenarioArgs}.

-spec add_default_processors(processors()) -> processors().
add_default_processors(Processor) ->
    Default = #{
        add_events => fun add_events/2
    },
    maps:merge(Default, Processor).

-spec apply_processor(processor(), scenario_args(), machine()) ->
    {ok, {scenario_response(), ff_machine:result(event())}} | {error, scenario_error()}.
apply_processor(Processor, Args, Machine) ->
    do(fun() ->
        {Response, #{events := Events} = Result} = unwrap(Processor(Args, Machine)),
        {Response, Result#{events => ff_machine:emit_events(Events)}}
    end).

-spec validate_result(module(), machine(), result()) -> {ok, valid} | {error, invalid_result_error()}.
validate_result(Mod, #{history := History} = Machine, #{events := NewEvents}) ->
    HistoryLen = erlang:length(History),
    NewEventsLen = erlang:length(NewEvents),
    IDs = lists:seq(HistoryLen + 1, HistoryLen + NewEventsLen),
    NewHistory = [{ID, machinery_time:now(), Event} || {ID, Event} <- lists:zip(IDs, NewEvents)],
    try
        _ = ff_machine:collapse(Mod, Machine#{history => History ++ NewHistory}),
        {ok, valid}
    catch
        error:Error:Stack ->
            Stacktrace = genlib_format:format_stacktrace(Stack),
            logger:warning("Invalid repair result: ~p, Stack: ~p", [Error, Stacktrace], #{
                error => genlib:format(Error),
                stacktrace => Stacktrace
            }),
            {error, unexpected_failure}
    end.

-spec add_events(scenario_result(), machine()) -> {ok, {ok, scenario_result()}}.
add_events(Result, _Machine) ->
    {ok, {ok, Result}}.
