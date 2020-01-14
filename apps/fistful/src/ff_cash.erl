-module(ff_cash).

-type amount() :: integer().
-type cash() :: {amount(), ff_currency:id()}.

-export_type([amount/0]).
-export_type([cash/0]).
