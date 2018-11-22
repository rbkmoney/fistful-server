%%%
%%% Checks idempotency of input data
%%%

-module(ff_checker).

-behaviour(machinery).

%% API
-type check_result() ::
    {ok, continue} |
    {ok, accepted} |
    {error, {check_failed, hash()}}.

-type params() :: #{
    entity_name := entity_name(),
    external_id := binary(),
    party_id    := binary(),
    request     := request_data()
}.

-export([check/1]).

-export_type([params/0]).
-export_type([check_result/0]).

%% Machinery

-export([init/4]).
-export([process_timeout/3]).
-export([process_call/4]).

-define(NS, 'ff/checker').

%%

-type hash() :: binary().

-type entity_name()  :: identity | wallet | withdrawal | deposit | source | destination.
-type request_data() :: {operation_id(), #{atom() | binary() => term()}}.
-type operation_id() :: atom().

%% API

-spec check(params()) ->
    check_result().

check(Params = #{request := Request}) ->
    ID = create_id(Params),
    Hash = get_request_hash(erlang:term_to_binary(Request)),
    case machinery:call(?NS, ID, {undefined, 0, forward}, dummy_call, backend()) of
        {ok, OldHash} ->
            compare_hash(Hash, OldHash);
        {error, notfound} ->
            _ = machinery:start(?NS, ID, Hash, backend()),
            {ok, continue}
    end.

%% Machinery

-type ev() :: empty.

-type auxst() ::
    binary().

-type machine()      :: machinery:machine(ev(), auxst()).
-type result()       :: machinery:result(ev(), auxst()).
-type handler_opts() :: machinery:handler_opts(_).
-type id()           :: machinery:id().

-spec init(hash(), machine(), _, handler_opts()) ->
    result().

-spec process_timeout(machine(), _, handler_opts()) ->
    result().

-type call() :: dummy_call.

-spec process_call(call(), machine(), _, handler_opts()) ->
    {hash(), result()}.

init(Hash, #{}, _, _Opts) ->
    #{
        aux_state => Hash
    }.

process_timeout(#{}, _, _Opts) ->
    #{}.

process_call(dummy_call, #{aux_state := Hash}, _, _Opts) ->
    {Hash, #{}}.

%%

-spec create_id(params()) ->
    id().

create_id(#{
    entity_name := EntityName,
    external_id := ExternalID,
    party_id    := PartyID
}) ->
    Name = erlang:term_to_binary(EntityName),
    <<"ff/eid/", Name/binary, "/", PartyID/binary, "/", ExternalID/binary>>.

get_request_hash(Data) ->
    crypto:hash(sha224, Data).

compare_hash(Hash, Hash) ->
    {ok, accepted};
compare_hash(Hash, _OldHash) ->
    {error, {check_failed, Hash}}.

backend() ->
    fistful:backend(?NS).
