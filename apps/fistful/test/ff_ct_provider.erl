-module(ff_ct_provider).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_withdrawals_provider_adapter_thrift.hrl").

%% API
-export([start/0]).
-export([start/1]).

%% Processing callbacks
-export([process_withdrawal/3]).
-export([get_quote/2]).
-export([handle_callback/4]).

-define(DUMMY_QUOTE, {obj, #{{str, <<"test">>} => {str, <<"test">>}}}).
-define(DUMMY_QUOTE_ERROR, {obj, #{{str, <<"test">>} => {str, <<"error">>}}}).

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
process_withdrawal(#{quote := #wthadpt_Quote{quote_data = QuoteData}}, State, _Options) when
    QuoteData =:= ?DUMMY_QUOTE_ERROR
->
    {ok, #{
        intent => {finish, {failure, <<"test_error">>}},
        next_state => State
    }};
process_withdrawal(#{quote := #wthadpt_Quote{quote_data = QuoteData}}, State, _Options) when
    QuoteData =:= ?DUMMY_QUOTE
->
    {ok, #{
        intent => {finish, {success, #{id => <<"test">>}}},
        next_state => State
    }};
process_withdrawal(_Withdrawal, State, _Options) ->
    {ok, #{
        intent => {finish, {success, #{id => <<"test">>}}},
        next_state => State
    }}.

-dialyzer({nowarn_function, get_quote/2}).

-spec get_quote(quote_params(), map()) -> {ok, quote()}.
get_quote(
    #{
        currency_from := CurrencyFrom,
        currency_to := CurrencyTo,
        exchange_cash := #wthadpt_Cash{amount = Amount, currency = Currency}
    },
    _Options
) ->
    {ok, #{
        cash_from => calc_cash(CurrencyFrom, Currency, Amount),
        cash_to => calc_cash(CurrencyTo, Currency, Amount),
        created_at => ff_time:to_rfc3339(ff_time:now()),
        expires_on => ff_time:to_rfc3339(ff_time:now() + 15 * 3600 * 1000),
        quote_data => ?DUMMY_QUOTE
    }}.

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

calc_cash(Currency, Currency, Amount) ->
    #wthadpt_Cash{amount = Amount, currency = Currency};
calc_cash(Currency, _, Amount) ->
    NewAmount = erlang:round(Amount / 2),
    #wthadpt_Cash{amount = NewAmount, currency = Currency}.
