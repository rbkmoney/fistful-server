-module(ff_services).

-export([get_service/1]).
-export([get_service_path/1]).
-export([get_service_spec/1]).

-export_type([service/0]).
-export_type([service_name/0]).
-export_type([service_spec/0]).

%%

-type service()      :: woody:service().
-type service_name() :: atom().
-type service_spec() :: {Path :: string(), service()}.

-spec get_service(service_name()) -> service().
get_service(fistful_admin) ->
    {ff_proto_fistful_admin_thrift, 'FistfulAdmin'};
get_service(ff_p2p_adapter_host) ->
    {dmsl_p2p_adapter_thrift, 'P2PAdapterHost'};
get_service(deposit_event_sink) ->
    {ff_proto_deposit_thrift, 'EventSink'};
get_service(source_event_sink) ->
    {ff_proto_source_thrift, 'EventSink'};
get_service(destination_event_sink) ->
    {ff_proto_destination_thrift, 'EventSink'};
get_service(identity_event_sink) ->
    {ff_proto_identity_thrift, 'EventSink'};
get_service(wallet_event_sink) ->
    {ff_proto_wallet_thrift, 'EventSink'};
get_service(withdrawal_event_sink) ->
    {ff_proto_withdrawal_thrift, 'EventSink'};
get_service(withdrawal_session_event_sink) ->
    {ff_proto_withdrawal_session_thrift, 'EventSink'};
get_service(withdrawal_session_repairer) ->
    {ff_proto_withdrawal_session_thrift, 'Repairer'};
get_service(withdrawal_repairer) ->
    {ff_proto_withdrawal_thrift, 'Repairer'};
get_service(deposit_repairer) ->
    {ff_proto_deposit_thrift, 'Repairer'};
get_service(wallet_management) ->
    {ff_proto_wallet_thrift, 'Management'};
get_service(identity_management) ->
    {ff_proto_identity_thrift, 'Management'};
get_service(destination_management) ->
    {ff_proto_destination_thrift, 'Management'};
get_service(withdrawal_management) ->
    {ff_proto_withdrawal_thrift, 'Management'};
get_service(deposit_management) ->
    {ff_proto_deposit_thrift, 'Management'};
get_service(p2p_transfer_event_sink) ->
    {ff_proto_p2p_transfer_thrift, 'EventSink'};
get_service(p2p_session_event_sink) ->
    {ff_proto_p2p_session_thrift, 'EventSink'};
get_service(p2p_transfer_repairer) ->
    {ff_proto_p2p_transfer_thrift, 'Repairer'};
get_service(p2p_session_repairer) ->
    {ff_proto_p2p_session_thrift, 'Repairer'}.

-spec get_service_spec(service_name()) -> service_spec().
get_service_spec(Name) ->
    {get_service_path(Name), get_service(Name)}.

-spec get_service_path(service_name()) -> string().
get_service_path(fistful_admin) ->
    "/v1/admin";
get_service_path(ff_p2p_adapter_host) ->
    "/v1/ff_p2p_adapter_host";
get_service_path(deposit_event_sink) ->
    "/v1/eventsink/deposit";
get_service_path(source_event_sink) ->
    "/v1/eventsink/source";
get_service_path(destination_event_sink) ->
    "/v1/eventsink/destination";
get_service_path(identity_event_sink) ->
    "/v1/eventsink/identity";
get_service_path(wallet_event_sink) ->
    "/v1/eventsink/wallet";
get_service_path(withdrawal_event_sink) ->
    "/v1/eventsink/withdrawal";
get_service_path(withdrawal_session_event_sink) ->
    "/v1/eventsink/withdrawal/session";
get_service_path(withdrawal_session_repairer) ->
    "/v1/repair/withdrawal/session";
get_service_path(withdrawal_repairer) ->
    "/v1/repair/withdrawal";
get_service_path(deposit_repairer) ->
    "/v1/repair/deposit";
get_service_path(wallet_management) ->
    "/v1/wallet";
get_service_path(identity_management) ->
    "/v1/identity";
get_service_path(destination_management) ->
    "/v1/destination";
get_service_path(withdrawal_management) ->
    "/v1/withdrawal";
get_service_path(deposit_management) ->
    "/v1/deposit";
get_service_path(p2p_transfer_event_sink) ->
    "/v1/eventsink/p2p_transfer";
get_service_path(p2p_session_event_sink) ->
    "/v1/eventsink/p2p_transfer/session";
get_service_path(p2p_transfer_repairer) ->
    "/v1/repair/p2p_transfer";
get_service_path(p2p_session_repairer) ->
    "/v1/repair/p2p_transfer/session".
