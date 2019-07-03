-module(ff_ct_provider).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("dmsl/include/dmsl_withdrawals_provider_adapter_thrift.hrl").

%% API
-export([start/0]).
-export([start/1]).

%% Processing callbacks
-export([process_withdrawal/3]).
-export([get_quote/2]).

%%
%% Internal types
%%

-type destination() :: dmsl_withdrawals_domain_thrift:'Destination'().
-type identity() :: dmsl_withdrawals_domain_thrift:'Identity'().
-type cash() :: dmsl_domain_thrift:'Cash'().
-type currency() :: dmsl_domain_thrift:'Currency'().
-type failure() :: dmsl_domain_thrift:'Failure'().

-type withdrawal() :: #{
    id => binary(),
    body => cash(),
    destination => destination(),
    sender => identity(),
    receiver => identity()
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

-spec get_quote(quote_params(), map()) ->
    {ok, quote()}.
get_quote(#{
    currency_from := CurrencyFrom,
    currency_to := CurrencyTo,
    exchange_cash := #wthadpt_Cash{amount = Amount, currency = Currency}
}, _Options) ->
    {ok, #{
        cash_from => calc_cash(CurrencyFrom, Currency, Amount),
        cash_to => calc_cash(CurrencyTo, Currency, Amount),
        created_at => ff_time:to_rfc3339(ff_time:now()),
        expires_on => ff_time:to_rfc3339(ff_time:now() + 15*3600*1000),
        quote_data => {obj, #{{str, <<"test">>} => {str, <<"test">>}}}
    }}.

calc_cash(Currency, Currency, Amount) ->
    #wthadpt_Cash{amount = Amount, currency = Currency};
calc_cash(Currency, _, Amount) ->
    NewAmount = erlang:round(Amount / 2),
    #wthadpt_Cash{amount = NewAmount, currency = Currency}.
