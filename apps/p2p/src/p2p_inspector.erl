-module(p2p_inspector).

-type risk_score()      :: low | high | fatal.
-type scores()          :: #{binary() => risk_score()}.
-type inspector()       :: dmsl_domain_thrift:'P2PInspector'().
-type transfer()        :: p2p_transfer:p2p_transfer().
-type domain_revision() :: ff_domain_config:revision().

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_proxy_inspector_p2p_thrift.hrl").

-export_type([risk_score/0]).
-export_type([scores/0]).

-export([inspect/4]).

-spec inspect(transfer(), domain_revision(), [binary()], inspector()) -> scores().
inspect(P2PTransfer, DomainRevision, RiskTypes, Inspector) ->
    #domain_P2PInspector{
        fallback_risk_score = FallBackRiskScore,
        proxy = #domain_Proxy{
            ref = ProxyRef,
            additional = ProxyAdditional
        }
    } = Inspector,
    Adapter = get_adapter(ProxyRef, DomainRevision, ProxyAdditional),
    #{
        adapter := Client,
        options := Options} = Adapter,
    Request = create_request(P2PTransfer, RiskTypes, Options),

    case issue_call(Client, Request, FallBackRiskScore) of
        {ok, RiskScores}  ->
            RiskScores;
        {exception, Error} ->
            error(Error)
    end.

issue_call(Client, Request, undefined) ->
    case ff_woody_client:call(Client, Request) of
        {ok, InspectResult} ->
            {ok, decode_inspect_result(InspectResult)};
        {exception, _} = Error ->
            Error
    end;
issue_call(Client, Request, Default) ->
    try ff_woody_client:call(Client, Request) of
        {ok, InspectResult}  ->
            {ok, decode_inspect_result(InspectResult)};
        {exception, Error} ->
            _ = logger:error("Fail to get RiskScore with error ~p", [Error]),
            {ok, Default}
    catch
        error:{woody_error, {_Source, Class, _Details}} = Reason
            when Class =:= resource_unavailable orelse
                 Class =:= result_unknown ->
            _ = logger:warning("Fail to get RiskScore with error ~p:~p", [error, Reason]),
            {ok, Default};
        error:{woody_error, {_Source, result_unexpected, _Details}} = Reason ->
            _ = logger:error("Fail to get RiskScore with error ~p:~p", [error, Reason]),
            {ok, Default}
    end.

get_adapter(Ref, Revision, ProviderOpts) ->
    {ok, ProxyDef} = ff_domain_config:object(Revision, {proxy, Ref}),
    #domain_ProxyDefinition{
        url = URL,
        options = ProxyOpts
    } = ProxyDef,
    #{
        adapter => ff_woody_client:new(URL),
        options => maps:merge(ProviderOpts, ProxyOpts)
    }.

create_request(P2PTransfer, RiskTypes, Options) ->
    Context = #p2p_insp_Context{
        info = encode_transfer_info(P2PTransfer),
        options = Options
    },
    Args = [Context, RiskTypes],
    {{dmsl_proxy_inspector_p2p_thrift, 'InspectorProxy'}, 'InspectTransfer', Args}.

encode_transfer_info(P2PTransfer) ->
    ID = p2p_transfer:id(P2PTransfer),
    IdentityID = p2p_transfer:owner(P2PTransfer),
    CreatedAt = ff_time:to_rfc3339(p2p_transfer:created_at(P2PTransfer)),
    Cash = ff_dmsl_codec:marshal(cash, p2p_transfer:body(P2PTransfer)),
    Sender = p2p_transfer:sender_resource(P2PTransfer),
    SenderContactInfo = p2p_participant:contact_info(p2p_transfer:sender(P2PTransfer)),
    Receiver = p2p_transfer:receiver_resource(P2PTransfer),
    ReceiverContactInfo = p2p_participant:contact_info(p2p_transfer:receiver(P2PTransfer)),
    Transfer = #p2p_insp_Transfer{
        id = ID,
        identity = #p2p_insp_Identity{id = IdentityID},
        created_at = CreatedAt,
        sender = encode_raw(ff_dmsl_codec:marshal(payment_resource_payer, {Sender, SenderContactInfo})),
        receiver = encode_raw(ff_dmsl_codec:marshal(payment_resource_payer, {Receiver, ReceiverContactInfo})),
        cost = Cash
    },
    #p2p_insp_TransferInfo{transfer = Transfer}.

decode_inspect_result(InspectResult) ->
    #p2p_insp_InspectResult{scores = Scores} = InspectResult,
    Scores.

encode_raw(PaymentResource) ->
    {raw, #p2p_insp_Raw{
        payer = {payment_resource, PaymentResource}
    }}.
