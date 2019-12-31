%%%
%%% Manage external_id
%%%

-module(ff_external_id).

-behaviour(machinery).

%% API

-type check_result() :: {ok, sequence()}.

-type external_id()  :: binary() | undefined.

-export([check_in/2]).
-export([bind/3]).
-export([get_internal_id/2]).
-export([get_ff_internal_id/3]).

-export([construct_external_id/2]).

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

-spec bind(entity_name(), external_id(), sequence()) ->
    check_result().

bind(EntityName, ExternalID, InternalID) ->
    ID = create_id(EntityName, ExternalID),
    bind_(ID, InternalID).

-spec get_internal_id(entity_name(), external_id()) ->
    check_result() | {error, notfound}.

get_internal_id(EntityName, ExternalID) ->
    ID = create_id(EntityName, ExternalID),
    get_(ID).

-spec get_ff_internal_id(entity_name(), ff_party:id(), external_id()) ->
    check_result() | {error, notfound}.

get_ff_internal_id(Type, PartyID, ExternalID) ->
    FistfulID = construct_external_id(PartyID, ExternalID),
    get_internal_id(Type, FistfulID).

-spec construct_external_id(ff_party:id(), external_id()) ->
    external_id().

construct_external_id(_PartyID, undefined) ->
    undefined;
construct_external_id(PartyID, ExternalID) ->
    <<PartyID/binary, "/", ExternalID/binary>>.

%%

check_in_(EntityName, ID) ->
    case get_(ID) of
        {ok, _Seq} = Ok ->
            Ok;
        {error, notfound} ->
            NextID = next_id(EntityName),
            bind_(ID, NextID)
    end.

bind_(ID, Seq) ->
    case start_(ID, Seq) of
        {ok, Seq} = Ok->
            Ok;
        {error, exists} ->
            get_(ID)
    end.

%%

get_(ID) ->
    case machinery:get(?NS, ID, {undefined, 0, forward}, backend()) of
        {ok, #{aux_state := Seq}} ->
            {ok, Seq};
        {error, notfound} = Error ->
            Error
    end.

start_(ID, InternalID) ->
    case machinery:start(?NS, ID, InternalID, backend()) of
        ok ->
            {ok, InternalID};
        {error, exists} = Error ->
            Error
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
