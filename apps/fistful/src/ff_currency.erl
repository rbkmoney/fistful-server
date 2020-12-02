%%%
%%% Currency
%%%

-module(ff_currency).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%%

-type id() :: symcode().
-type symcode() :: binary().
-type currency() :: #{
    id := id(),
    name := binary(),
    symcode := symcode(),
    numcode := integer(),
    exponent := non_neg_integer(),
    sign => binary()
}.

-export_type([id/0]).
-export_type([currency/0]).

-export([get/1]).
-export([get/2]).
-export([symcode/1]).
-export([id/1]).
-export([to_domain_ref/1]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%% Accessors

-spec symcode(currency()) -> symcode().
symcode(#{symcode := SymCode}) ->
    SymCode.

-spec id(currency()) -> id().
id(#{id := ID}) ->
    ID.

%%

-spec get(id()) ->
    {ok, currency()}
    | {error, notfound}.
get(ID) ->
    get(ID, ff_domain_config:head()).

-spec get(id(), ff_domain_config:revision()) ->
    {ok, currency()}
    | {error, notfound}.
get(ID, DomainRevision) ->
    do(fun() ->
        Currency = unwrap(
            ff_domain_config:object(
                DomainRevision,
                {currency, #domain_CurrencyRef{symbolic_code = ID}}
            )
        ),
        #{
            id => ID,
            name => Currency#domain_Currency.name,
            symcode => Currency#domain_Currency.symbolic_code,
            numcode => Currency#domain_Currency.numeric_code,
            exponent => Currency#domain_Currency.exponent
        }
    end).

-spec to_domain_ref(currency()) -> dmsl_domain_thrift:'CurrencyRef'().
to_domain_ref(Currency) ->
    ff_dmsl_codec:marshal(currency_ref, ff_currency:id(Currency)).
