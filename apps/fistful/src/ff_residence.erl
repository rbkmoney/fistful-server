%%%
%%% Residence
%%%
%%% TODOs:
%%%  - Move it to some kind of domain config
%%%

-module(ff_residence).

%%

-type id()        :: atom().
-type residence() :: #{
    name          := binary(),
    flag          => binary()
}.

-export_type([id/0]).
-export_type([residence/0]).

-export([get/1]).

%%

-spec get(id()) ->
    residence().

get('rus') ->
    #{
        name => <<"Российская федерация">>,
        flag => <<"🇷🇺">>
    }.
