-module(ff_ct_fail_provider).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_withdrawals_provider_adapter_thrift.hrl").

%% API
-export([start/0]).
-export([start/1]).

%% Processing callbacks
-export([process_withdrawal/3]).
-export([get_quote/2]).
-export([handle_callback/4]).

%%
%% Internal types
%%

-type destination() :: dmsl_withdrawals_domain_thrift:'Destination'().
-type identity() :: dmsl_withdrawals_domain_thrift:'Identity'().
-type cash() :: dmsl_domain_thrift:'Cash'().
-type currency() :: dmsl_domain_thrift:'Currency'().
-type failure() :: dmsl_domain_thrift:'Failure'().
-type domain_quote() :: dmsl_withdrawals_provider_adapter_thrift:'Quote'().

-type withdrawal() :: #{
    id => binary(),
    body => cash(),
    destination => destination(),
    sender => identity(),
    receiver => identity(),
    quote => domain_quote()
}.

-type quote_params() :: #{
    idempotency_id => binary(),
    currency_from := currency(),
    currency_to := currency(),
    exchange_cash := cash()
}.

-type quote() :: #{
    cash_from := cash(),
    cash_to := cash(),
    created_at := binary(),
    expires_on := binary(),
    quote_data := any()
}.

-type callback() :: ff_withdrawal_callback:callback().

-record(state, {}).

-type state() :: #state{}.

-type transaction_info() :: ff_adapter:transaction_info().
-type status() :: {success, transaction_info()} | {failure, failure()}.
-type timer() :: {deadline, binary()} | {timeout, integer()}.

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

-spec process_withdrawal(withdrawal(), state(), map()) ->
    {ok, #{
        intent := {finish, status()} | {sleep, timer()} | {sleep, timer(), CallbackTag},
        next_state => state(),
        transaction_info => transaction_info()
    }}
when
    CallbackTag :: binary().
process_withdrawal(_Withdrawal, State, _Options) ->
    {ok, #{
        intent => {finish, {failure, <<"authorization_error">>}},
        next_state => State
    }}.

-dialyzer({nowarn_function, get_quote/2}).

-spec get_quote(quote_params(), map()) -> {ok, quote()}.
get_quote(_Quote, _Options) ->
    erlang:error(not_implemented).

-dialyzer({nowarn_function, handle_callback/4}).

-spec handle_callback(callback(), withdrawal(), state(), map()) ->
    {ok, #{
        intent := {finish, status()} | {sleep, timer()} | {sleep, timer(), CallbackTag},
        response := any(),
        next_state => state(),
        transaction_info => transaction_info()
    }}
when
    CallbackTag :: binary().
handle_callback(_Callback, _Withdrawal, _State, _Options) ->
    erlang:error(not_implemented).
