%%%
%%% Machinery gen_server backend
%%%
%%% A supervisor for machine processes.
%%%

-module(machinery_gensrv_backend_sup).

%% API

-export([start_link/2]).

%% Supervisor

-behaviour(supervisor).

-export([init/1]).

%%

-type sup_name() :: {via, module(), any()}.
-type mfargs() :: {module(), atom(), [_]}.

-spec start_link(sup_name(), mfargs()) -> {ok, pid()} | {error, {already_started, pid()}}.
start_link(SupName, Args) ->
    supervisor:start_link(SupName, ?MODULE, Args).

%%

-spec init({supervisor, mfargs()}) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(MFA) ->
    {ok,
        {
            #{strategy => simple_one_for_one},
            [
                #{
                    id => machine,
                    start => MFA,
                    type => worker,
                    restart => temporary
                }
            ]
        }}.
