%%%
%%% Currency
%%%

-module(ff_currency).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

%%

-type id()        :: symcode().
-type symcode()   :: binary().
-type currency()  :: #{
    id            := id(),
    name          := binary(),
    symcode       := symcode(),
    numcode       := integer(),
    exponent      := non_neg_integer(),
    sign          => binary()
}.

-export_type([id/0]).
-export_type([currency/0]).

-export([get/1]).
-export([symcode/1]).
-export([id/1]).

%% Pipeline

-compile({parse_transform, ff_pipeline}).
-import(ff_pipeline, [unwrap/1]).

%% Accessors

-spec symcode(currency()) -> symcode().
symcode(#{symcode := SymCode}) ->
    SymCode.

-spec id(currency()) -> id().
id(#{id := ID}) ->
    ID.

%%

-spec get(id()) ->
    {ok, currency()} |
    {error, notfound}.

get(ID) ->
    ff_pipeline:do(fun () ->
        Currency = unwrap(ff_domain_config:object({currency, #domain_CurrencyRef{symbolic_code = ID}})),
        #{
            id       => ID,
            name     => Currency#domain_Currency.name,
            symcode  => Currency#domain_Currency.symbolic_code,
            numcode  => Currency#domain_Currency.numeric_code,
            exponent => Currency#domain_Currency.exponent
        }
    end).
