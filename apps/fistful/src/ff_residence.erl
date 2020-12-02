%%%
%%% Residence
%%%
%%% TODOs:
%%%  - Move it to some kind of domain config
%%%

-module(ff_residence).

%%

-type id() :: dmsl_domain_thrift:'Residence'().
-type residence() :: #{
    id := id(),
    name := binary(),
    flag => binary()
}.

-export_type([id/0]).
-export_type([residence/0]).

-export([get/1]).

%%

-spec get(id()) -> ff_map:result(residence()).
get(ID = 'rus') ->
    {ok, #{
        id => ID,
        name => <<"Российская федерация"/utf8>>,
        flag => <<"🇷🇺"/utf8>>
    }};
get(_) ->
    {error, notfound}.
