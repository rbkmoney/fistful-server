-module(ff_repair).

-export([apply_repair/3]).
-export([apply_repair/4]).

%% Types

-type scenario() ::
    scenario_id() |
    {scenario_id(), scenario_args()}.

-type scenario_id() :: atom().
-type scenario_args() :: any().

-type processor() :: fun((scenario_args(), machine()) -> result()).
-type processors() :: #{
    scenario_id() := processor()
}.

-export_type([scenario/0]).
-export_type([scenario_id/0]).
-export_type([scenario_args/0]).
-export_type([processor/0]).
-export_type([processors/0]).

%% Internal types

-type event() :: ff_machine:timestamped_event(any()).
-type result() :: machinery:result(event()).
-type machine() :: ff_machine:machine(event()).

%% API

-spec apply_repair(module(), machine(), scenario()) ->
    result().
apply_repair(Mod, Machine, Scenario) ->
    apply_repair(Mod, Machine, Scenario, #{}).

-spec apply_repair(module(), machine(), scenario(), processors()) ->
    result().
apply_repair(Mod, Machine, Scenario, Processors) ->
    {ScenarioID, ScenarioArgs} = unwrap_scenario(Scenario),
    AllProcessors = add_default_processors(Processors),
    case maps:find(ScenarioID, AllProcessors) of
        {ok, Processor} ->
            Result = apply_processor(Processor, ScenarioArgs, Machine),
            valid = validate_result(Mod, Machine, Result),
            Result;
        error ->
            erlang:error({unknown_scenario, {ScenarioID, maps:keys(AllProcessors)}})
    end.

%% Internals

-spec unwrap_scenario(scenario()) ->
    {scenario_id(), scenario_args()}.
unwrap_scenario(ScenarioID) when is_atom(ScenarioID) ->
    {ScenarioID, undefined};
unwrap_scenario({ScenarioID, ScenarioArgs}) when is_atom(ScenarioID) ->
    {ScenarioID, ScenarioArgs}.

-spec add_default_processors(processors()) ->
    processors().
add_default_processors(Processor) ->
    Default = #{
        add_events => fun add_events/2
    },
    maps:merge(Default, Processor).

-spec apply_processor(processor(), scenario_args(), machine()) ->
    ff_machine:result(event()).
apply_processor(Processor, Args, Machine) ->
    #{events := Events} = Result = Processor(Args, Machine),
    Result#{events => ff_machine:emit_events(Events)}.

-spec validate_result(module(), machine(), result()) ->
    valid | no_return().
validate_result(Mod, #{history := History} = Machine, #{events := NewEvents}) ->
    HistoryLen = erlang:length(History),
    NewEventsLen = erlang:length(NewEvents),
    IDs = lists:seq(HistoryLen + 1, HistoryLen + NewEventsLen),
    NewHistory = [{ID, machinery_time:now(), Event} || {ID, Event} <- lists:zip(IDs, NewEvents)],
    _ = ff_machine:collapse(Mod, Machine#{history => History ++ NewHistory}),
    valid.

-spec add_events(result(), machine()) ->
    result().
add_events(Result, _Machine) ->
    Result.
