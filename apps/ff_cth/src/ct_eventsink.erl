-module(ct_eventsink).

-include_lib("fistful_proto/include/ff_proto_identity_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_wallet_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_withdrawal_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_withdrawal_session_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_destination_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_source_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_deposit_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_p2p_transfer_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_p2p_session_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_w2w_transfer_thrift.hrl").
-include_lib("fistful_proto/include/ff_proto_p2p_template_thrift.hrl").

-type sink() ::
    ff_services:service_name().

-type event() ::
    ff_proto_wallet_thrift:'SinkEvent'()
    | ff_proto_withdrawal_thrift:'SinkEvent'()
    | ff_proto_identity_thrift:'SinkEvent'()
    | ff_proto_destination_thrift:'SinkEvent'()
    | ff_proto_source_thrift:'SinkEvent'()
    | ff_proto_deposit_thrift:'SinkEvent'()
    | ff_proto_withdrawal_thrift:'SinkEvent'()
    | ff_proto_p2p_transfer_thrift:'SinkEvent'()
    | ff_proto_p2p_session_thrift:'SinkEvent'()
    | ff_proto_w2w_transfer_thrift:'SinkEvent'()
    | ff_proto_p2p_template_thrift:'SinkEvent'().

-type event_id() :: ff_proto_eventsink_thrift:'EventID'().
-type limit() :: non_neg_integer().

-export([last_id/1]).

-export([events/3]).
-export([consume/2]).
-export([fold/4]).

-export([get_max_event_id/1]).

%%

-spec last_id(sink()) -> event_id() | 0.
last_id(Sink) ->
    case call_handler('GetLastEventID', Sink, {}) of
        {ok, EventID} ->
            EventID;
        {exception, #'evsink_NoLastEvent'{}} ->
            0
    end.

-spec events(_After :: event_id() | undefined, limit(), sink()) -> {[event()], _Last :: event_id()}.
events(After, Limit, Sink) ->
    Range = #'evsink_EventRange'{'after' = After, limit = Limit},
    {ok, Events} = call_handler('GetEvents', Sink, {Range}),
    {Events, get_max_event_id(Events)}.

-spec consume(_ChunkSize :: limit(), sink()) -> [event()].
consume(ChunkSize, Sink) ->
    fold(fun(Chunk, Acc) -> Chunk ++ Acc end, [], ChunkSize, Sink).

-spec fold(fun(([event()], State) -> State), State, _ChunkSize :: limit(), sink()) -> State.
fold(FoldFun, InitialState, ChunkSize, Sink) ->
    fold(FoldFun, InitialState, ChunkSize, Sink, undefined).

fold(FoldFun, State0, ChunkSize, Sink, Cursor) ->
    {Events, LastID} = events(Cursor, ChunkSize, Sink),
    State1 = FoldFun(Events, State0),
    case length(Events) of
        N when N >= ChunkSize ->
            fold(FoldFun, State1, ChunkSize, Sink, LastID);
        _ ->
            State1
    end.

-spec get_max_event_id([event()]) -> event_id().
get_max_event_id(Events) when is_list(Events) ->
    lists:foldl(fun(Ev, Max) -> erlang:max(get_event_id(Ev), Max) end, 0, Events).

-spec get_event_id(event()) -> event_id().
get_event_id(#'wlt_SinkEvent'{id = ID}) -> ID;
get_event_id(#'wthd_SinkEvent'{id = ID}) -> ID;
get_event_id(#'idnt_SinkEvent'{id = ID}) -> ID;
get_event_id(#'dst_SinkEvent'{id = ID}) -> ID;
get_event_id(#'src_SinkEvent'{id = ID}) -> ID;
get_event_id(#'deposit_SinkEvent'{id = ID}) -> ID;
get_event_id(#'wthd_session_SinkEvent'{id = ID}) -> ID;
get_event_id(#'p2p_transfer_SinkEvent'{id = ID}) -> ID;
get_event_id(#'p2p_session_SinkEvent'{id = ID}) -> ID;
get_event_id(#'w2w_transfer_SinkEvent'{id = ID}) -> ID;
get_event_id(#'p2p_template_SinkEvent'{id = ID}) -> ID.

call_handler(Function, ServiceName, Args) ->
    Service = ff_services:get_service(ServiceName),
    Path = erlang:list_to_binary(ff_services:get_service_path(ServiceName)),
    Request = {Service, Function, Args},
    Client = ff_woody_client:new(#{
        url => <<"http://localhost:8022", Path/binary>>,
        event_handler => scoper_woody_event_handler
    }),
    ff_woody_client:call(Client, Request).
