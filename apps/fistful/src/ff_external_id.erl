%%%
%%% Manage external_id
%%%

-module(ff_external_id).

-behaviour(machinery).

%% API

-type check_result() :: {ok, sequence()}.

-type external_id()  :: binary() | undefined.

-export([check_in/2]).

-export_type([external_id/0]).
-export_type([check_result/0]).

%% Machinery

-export([init/4]).
-export([process_timeout/3]).
-export([process_repair/4]).
-export([process_call/4]).

-define(NS, 'ff/external_id').

%%

-type entity_name()  ::
    identity |
    wallet |
    withdrawal |
    deposit  |
    source |
    destination |
    identity_challenge.

-type sequence()     :: binary().

%% API

-spec check_in(entity_name(), external_id()) ->
    check_result().

check_in(EntityName, undefined) ->
    {ok, next_id(EntityName)};
check_in(EntityName, ExternalID) ->
    ID = create_id(EntityName, ExternalID),
    check_in_(EntityName, ID).

check_in_(EntityName, ID) ->
    case machinery:get(?NS, ID, {undefined, 0, forward}, backend()) of
        {ok, #{aux_state := Seq}} ->
            {ok, Seq};
        {error, notfound} ->
            NextID = next_id(EntityName),
            start_(EntityName, ID, NextID)
    end.

start_(EntityName, ID, Seq) ->
    case machinery:start(?NS, ID, Seq, backend()) of
        ok ->
            {ok, Seq};
        {error, exists} ->
            check_in_(EntityName, ID)
    end.

%% Machinery

-type ev() :: empty.

-type auxst() ::
    sequence().

-type machine()      :: machinery:machine(ev(), auxst()).
-type result()       :: machinery:result(ev(), auxst()).
-type handler_opts() :: machinery:handler_opts(_).
-type handler_args() :: machinery:handler_args(_).
-type id()           :: machinery:id().

-spec init(sequence(), machine(), handler_args(), handler_opts()) ->
    result().

-spec process_timeout(machine(), handler_args(), handler_opts()) ->
    result().

-spec process_call(_, machine(), handler_args(), handler_opts()) ->
    {_, result()}.

-spec process_repair(ff_repair:scenario(), machine(), handler_args(), handler_opts()) ->
    no_return().

init(Data, #{}, _, _Opts) ->
    #{
        aux_state => Data
    }.

process_timeout(#{}, _, _Opts) ->
    #{}.

process_call(_, #{}, _, _Opts) ->
    {ok, #{}}.

process_repair(_RepairArgs, _Machine, _Args, _Opts) ->
    erlang:error({not_implemented, repair}).

%%

-spec create_id(entity_name(), external_id()) ->
    id().

create_id(EntityName, ExternalID) ->
    Name = erlang:term_to_binary(EntityName),
    <<Name/binary, "/", ExternalID/binary>>.

backend() ->
    fistful:backend(?NS).

next_id(Type) ->
    NS = 'ff/sequence',
    erlang:integer_to_binary(
        ff_sequence:next(NS, ff_string:join($/, [Type, id]), fistful:backend(NS))
    ).