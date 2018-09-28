%%%
%%% Transfer session behaviour
%%%

-module(ff_transfer_session).

%% Behaviour definition
-type id(T) :: T.

-type data() :: #{
    id       := id(_),
    cash     := ff_transaction:body(),
    sender   := ff_identity:identity(),
    receiver := ff_identity:identity()
}.

-type params(T) :: T.

-type status() :: active
    | {finished, {success, _} | {failed, _}}.
-type session(T) :: T.

-export_type([data/0]).
-export_type([params/1]).
-export_type([session/1]).
-export_type([status/0]).

-callback create(id(_), data(), params(_)) ->
    ok | {error, exists}.

-callback status(session(_)) ->
    status().

-callback get(id(_)) ->
    {ok, session(_)} | {error, notfound}.

%% API

-export([create/4]).
-export([get/2]).
-export([status/2]).

-type type() :: ff_transfer:type().

-spec create(type(), id(_), data(), params(_)) ->
    ok | {error, exists}.

create(Type, ID, Data, Params) ->
    Handler = dispatch_session(Type),
    Handler:create(ID, Data, Params).

-spec get(type(), id(_)) ->
    {ok, session(_)} | {error, notfound}.

get(Type, ID) ->
    Handler = dispatch_session(Type),
    Handler:get(ID).

-spec status(type(), session(_)) ->
    status().

status(Type, Session) ->
    Handler = dispatch_session(Type),
    Handler:status(Session).

%%

dispatch_session(withdrawal) ->
    ff_withdrawal_session_machine.
%% dispatch_session(deposit) ->
%%     ff_deposit_session_machine.
