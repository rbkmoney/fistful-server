%%%
%%% Server startup
%%%

-module(ff_server).

-export([start/0]).

%% Application

-behaviour(application).

-export([start/2]).
-export([stop/1]).

%% Supervisor

-behaviour(supervisor).

-export([init/1]).

%%

-spec start() ->
    {ok, _}.

start() ->
    application:ensure_all_started(?MODULE).

%% Application

-spec start(normal, any()) ->
    {ok, pid()} | {error, any()}.

start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec stop(any()) ->
    ok.

stop(_State) ->
    ok.

%% Supervisor

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    % TODO
    %  - Make it palatable
    {Backends1, ChildSpecs1} = lists:unzip([
        contruct_backend_childspec('identity'           , ff_identity_machine),
        contruct_backend_childspec('wallet'             , ff_wallet_machine)
    ]),
    {Backends2, ChildSpecs2} = lists:unzip([
        contruct_backend_childspec('destination'        , ff_destination_machine),
        contruct_backend_childspec('withdrawal'         , ff_withdrawal_machine),
        contruct_backend_childspec('withdrawal/session' , ff_withdrawal_session_machine)
    ]),
    ok = application:set_env(fistful, backends, Backends1),
    ok = application:set_env(ff_withdraw, backends, Backends2),
    {ok,
        #{strategy => one_for_one},
        ChildSpecs1 ++ ChildSpecs2
    }.

contruct_backend_childspec(NS, Handler) ->
    Opts = #{name => NS},
    {
        {NS, machinery_gensrv_backend:new(Opts)},
        machinery_gensrv_backend:child_spec(Handler, Opts)
    }.
