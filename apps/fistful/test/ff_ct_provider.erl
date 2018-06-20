-module(ff_ct_provider).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

%% API
-export([start/0]).
-export([start/1]).

%% Processing callbacks
-export([process_withdrawal/3]).

%%
%% Internal types
%%

-type destination() :: dmsl_withdrawals_domain_thrift:'Destination'().
-type identity() :: dmsl_withdrawals_domain_thrift:'Identity'().
-type cash() :: dmsl_domain_thrift:'Cash'().
-type failure() :: dmsl_domain_thrift:'Failure'().

-type withdrawal() :: #{
    id => binary(),
    body => cash(),
    destination => destination(),
    sender => identity(),
    receiver => identity()
}.

-record(state, {}).
-type state() :: #state{}.

%%
%% API
%%

-spec start() -> {ok, pid()}.
start() ->
    start([]).

-spec start(list()) -> {ok, pid()}.
start(Opts) ->
    {ok, Pid} = supervisor:start_link(ff_ct_provider_sup, Opts),
    _ = erlang:unlink(Pid),
    {ok, Pid}.

%%
%% Processing callbacks
%%

-spec process_withdrawal(withdrawal(), state(), map()) -> {finish, Status} | {sleep, Timer} when
    Status :: {success, TrxInfo} | {failure, failure()},
    Timer :: {deadline, binary()} | {timeout, integer()},
    TrxInfo :: #{id => binary()}.
process_withdrawal(_Withdrawal, _State, _Options) ->
    {finish, {success, #{id => <<"test">>}}}.
