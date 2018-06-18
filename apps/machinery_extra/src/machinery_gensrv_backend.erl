%%%
%%% Machinery gen_server backend
%%%
%%% TODO
%%%  - Notion of _failed_ machines

-module(machinery_gensrv_backend).

-compile([{parse_transform, lager_transform}]).

-type namespace()       :: machinery:namespace().
-type id()              :: machinery:id().
-type range()           :: machinery:range().
-type machine(E, A)     :: machinery:machine(E, A).
-type args(T)           :: machinery:args(T).
-type response(T)       :: machinery:response(T).
-type logic_handler(T)  :: machinery:logic_handler(T).
-type timestamp()       :: machinery:timestamp().

-type backend_opts()    :: machinery:backend_opts(#{
    name                := atom()
}).

-type backend() :: {?MODULE, backend_opts()}.

-export_type([backend_opts/0]).
-export_type([backend/0]).

%% API

-export([new/1]).
-export([child_spec/2]).

%% Machinery backend

-behaviour(machinery_backend).

-export([start/4]).
-export([call/5]).
-export([get/4]).

%% Gen Server

-behaviour(gen_server).

-export([start_machine_link/4]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

%% API

-spec new(backend_opts()) ->
    backend().
new(Opts = #{name := _}) ->
    {?MODULE, Opts}.

-spec child_spec(logic_handler(_), backend_opts()) ->
    supervisor:child_spec().
child_spec(Handler0, Opts) ->
    Handler = machinery_utils:get_handler(Handler0),
    MFA = {?MODULE, start_machine_link, [Handler]},
    #{
        id    => get_sup_name(Opts),
        start => {machinery_gensrv_backend_sup, start_link, [get_sup_ref(Opts), MFA]},
        type  => supervisor
    }.

%% Machinery backend

-spec start(namespace(), id(), args(_), backend_opts()) ->
    ok | {error, exists}.
start(NS, ID, Args, Opts) ->
    _ = lager:debug("[machinery/gensrv][client][~s:~s] starting with args: ~p", [NS, ID, Args]),
    case supervisor:start_child(get_sup_ref(Opts), [NS, ID, Args]) of
        {ok, PID} ->
            _ = lager:debug("[machinery/gensrv][client][~s:~s] started as: ~p", [NS, ID, PID]),
            ok;
        {error, {already_started, _}} ->
            report_exists(NS, ID);
        {error, already_present} ->
            report_exists(NS, ID)
    end.

-spec call(namespace(), id(), range(), args(_), backend_opts()) ->
    {ok, response(_)} | {error, notfound}.
call(NS, ID, Range, Args, _Opts) ->
    _ = lager:debug("[machinery/gensrv][client][~s:~s] calling with range ~p and args: ~p", [NS, ID, Range, Args]),
    try gen_server:call(get_machine_ref(NS, ID), {call, Range, Args}) of
        Response ->
            _ = lager:debug("[machinery/gensrv][client][~s:~s] response: ~p", [NS, ID, Response]),
            {ok, Response}
    catch
        exit:noproc ->
            report_notfound(NS, ID);
        exit:{noproc, {gen_server, call, _}} ->
            report_notfound(NS, ID)
    end.

-spec get(namespace(), id(), range(), backend_opts()) ->
    {ok, machine(_, _)} | {error, notfound}.
get(NS, ID, Range, _Opts) ->
    _ = lager:debug("[machinery/gensrv][client][~s:~s] getting with range: ~p", [NS, ID, Range]),
    try gen_server:call(get_machine_ref(NS, ID), {get, Range}) of
        Machine ->
            _ = lager:debug("[machinery/gensrv][client][~s:~s] machine: ~p", [NS, ID, Machine]),
            {ok, Machine}
    catch
        exit:noproc ->
            report_notfound(NS, ID);
        exit:{noproc, {gen_server, call, _}} ->
            report_notfound(NS, ID)
    end.

report_exists(NS, ID) ->
    _ = _ = lager:debug("[machinery/gensrv][client][~s:~s] exists already", [NS, ID]),
    {error, exists}.

report_notfound(NS, ID) ->
    _ = _ = lager:debug("[machinery/gensrv][client][~s:~s] not found", [NS, ID]),
    {error, notfound}.

%% Gen Server + Supervisor

-spec start_machine_link(logic_handler(_), namespace(), id(), args(_)) ->
    {ok, pid()}.

start_machine_link(Handler, NS, ID, Args) ->
    gen_server:start_link(get_machine_ref(NS, ID), ?MODULE, {machine, Handler, NS, ID, Args}, []).

-type st(E, Aux, Args) :: #{
    machine  := machine(E, Aux),
    handler  := logic_handler(Args),
    deadline => timestamp()
}.

-spec init({machine, logic_handler(Args), namespace(), id(), args(_)}) ->
    ignore |
    {ok, st(_, _, Args), timeout()}.

init({machine, Handler, NS, ID, Args}) -> % Gen Server
    St0 = #{machine => construct_machine(NS, ID), handler => Handler},
    _ = lager:debug("[machinery/gensrv][server][~s:~s] dispatching init: ~p with state: ~p", [NS, ID, Args, St0]),
    Result = dispatch_signal({init, Args}, St0),
    case apply_result(Result, St0) of
        St1 = #{} ->
            _ = lager:debug("[machinery/gensrv][server][~s:~s] started with: ~p", [NS, ID, St1]),
            {ok, St1, compute_timeout(St1)};
        removed ->
            _ = lager:debug("[machinery/gensrv][server][~s:~s] removed", [NS, ID]),
            ignore
    end.

construct_machine(NS, ID) ->
    #{
        namespace => NS,
        id        => ID,
        history   => [],
        aux_state => undefined
    }.

-spec handle_call({call, range(), args(_)}, {pid(), reference()}, st(E, Aux, Args)) ->
    {reply, response(_), st(E, Aux, Args), timeout()} |
    {stop, normal, st(E, Aux, Args)}.

handle_call({call, Range, Args}, _From, St0 = #{machine := #{namespace := NS, id := ID}}) ->
    St1 = apply_range(Range, St0),
    _ = lager:debug("[machinery/gensrv][server][~s:~s] dispatching call: ~p with state: ~p", [NS, ID, Args, St1]),
    {Response, Result} = dispatch_call(Args, St0),
    case apply_result(Result, St0) of
        St2 = #{} ->
            _ = lager:debug("[machinery/gensrv][server][~s:~s] responded: ~p, new state: ~p", [NS, ID, Response, St2]),
            {reply, Response, St2, compute_timeout(St2)};
        removed ->
            _ = lager:debug("[machinery/gensrv][server][~s:~s] responded: ~p, removed", [NS, ID, Response]),
            {stop, normal, Response, St0}
    end;
handle_call({get, Range}, _From, St = #{machine := M}) ->
    {reply, apply_range(Range, M), St, compute_timeout(St)};
handle_call(Call, _From, _St) ->
    error({badcall, Call}).

-spec handle_cast(_Cast, st(_, _, _)) ->
    no_return().

handle_cast(Cast, _St) ->
    error({badcast, Cast}).

-spec handle_info(timeout, st(E, Aux, Args)) ->
    {noreply, st(E, Aux, Args), timeout()} |
    {stop, normal, st(E, Aux, Args)}.

handle_info(timeout, St0 = #{machine := #{namespace := NS, id := ID}}) ->
    _ = lager:debug("[machinery/gensrv][server][~s:~s] dispatching timeout with state: ~p", [NS, ID, St0]),
    Result = dispatch_signal(timeout, St0),
    case apply_result(Result, St0) of
        St1 = #{} ->
            _ = lager:debug("[machinery/gensrv][server][~s:~s] new state: ~p", [NS, ID, St1]),
            {noreply, St1, compute_timeout(St1)};
        removed ->
            _ = lager:debug("[machinery/gensrv][server][~s:~s] removed", [NS, ID]),
            {stop, normal, St0}
    end;
handle_info(Info, _St) ->
    error({badinfo, Info}).

apply_range(Range, St = #{machine := M}) ->
    St#{machine := apply_range(Range, M)};
apply_range(Range, M = #{history := H}) ->
    M#{history := select_range(Range, H)}.

apply_result(R = #{action := As}, St) ->
    apply_result(
        maps:remove(action, R),
        apply_actions(As, St)
    );
apply_result(R = #{events := Es}, St = #{machine := M}) ->
    apply_result(
        maps:remove(events, R),
        St#{machine := apply_events(Es, M)}
    );
apply_result(R = #{aux_state := Aux}, St = #{machine := M}) ->
    apply_result(
        maps:remove(aux_state, R),
        St#{machine := apply_auxst(Aux, M)}
    );
apply_result(#{}, St) ->
    St.

apply_actions(As, St) when is_list(As) ->
    lists:foldl(fun apply_action/2, St, As);
apply_actions(A, St) ->
    apply_action(A, St).

apply_action({set_timer, Timer}, St) ->
    St#{deadline => compute_deadline(Timer)};
apply_action(unset_timer, St) ->
    maps:without([deadline], St);
apply_action(continue, St) ->
    St#{deadline => machinery_time:now()};
apply_action(remove, _St) ->
    removed.

apply_events(Es, M = #{history := Hs}) ->
    Ts = machinery_time:now(),
    Hl = length(Hs),
    M#{history := Hs ++ [
        {ID, Ts, Eb} ||
            {ID, Eb} <- lists:zip(lists:seq(Hl + 1, Hl + length(Es)), Es)
    ]}.

apply_auxst(Aux, M = #{}) ->
    M#{aux_state := Aux}.

compute_deadline({timeout, V}) ->
    machinery_time:add_seconds(V, machinery_time:now());
compute_deadline({deadline, V}) ->
    V.

compute_timeout(#{deadline := Deadline}) ->
    erlang:max(0, machinery_time:interval(Deadline, machinery_time:now()));
compute_timeout(#{}) ->
    infinity.

%% Utils

get_name(#{name := V}) ->
    V.

get_sup_ref(Opts) ->
    {via, gproc, construct_gproc_ref(get_sup_name(Opts))}.

get_sup_name(Opts) ->
    {?MODULE, {sup, get_name(Opts)}}.

get_machine_ref(NS, ID) ->
    {via, gproc, construct_gproc_ref(get_machine_name(NS, ID))}.

get_machine_name(NS, ID) ->
    {?MODULE, {machine, NS, ID}}.

construct_gproc_ref(Name) ->
    {n, l, Name}.

dispatch_signal(Signal, #{machine := Machine, handler := Handler}) ->
    machinery:dispatch_signal(Signal, Machine, Handler, undefined).

dispatch_call(Args, #{machine := Machine, handler := Handler}) ->
    machinery:dispatch_call(Args, Machine, Handler, undefined).

%%

select_range({Ec, N, Dir}, H) ->
    R0 = {1, length(H)},
    R1 = intersect_range(Ec, Dir, R0),
    R2 = limit_range(N, Dir, R1),
    query_range(R2, Dir, H).

intersect_range(undefined, _, R) ->
    R;
intersect_range(Ec, forward, {A, B}) ->
    {erlang:max(A, Ec + 1), B};
intersect_range(Ec, backward, {A, B}) ->
    {A, erlang:min(B, Ec - 1)}.

limit_range(undefined, _, R) ->
    R;
limit_range(N, forward, {A, B}) ->
    {A, min(A + N - 1, B)};
limit_range(N, backward, {A, B}) ->
    {max(B - N + 1, A), B}.

query_range({A, B}, _, _) when A > B ->
    [];
query_range({A, B}, forward, H) ->
    lists:sublist(H, A, B - A);
query_range({A, B}, backward, H) ->
    lists:reverse(lists:sublist(H, A, B - A)).
