-module(wapi_payres_handler).

-include_lib("dmsl/include/dmsl_cds_thrift.hrl").

-behaviour(swag_server_payres_logic_handler).
-behaviour(wapi_handler).

%% swag_server_payres_logic_handler callbacks
-export([authorize_api_key/2]).
-export([handle_request/3]).

%% wapi_handler callbacks
-export([process_request/3]).

%% Types

-type req_data()        :: wapi_handler:req_data().
-type handler_context() :: wapi_handler:handler_context().
-type request_result()  :: wapi_handler:request_result().

%% API

-spec authorize_api_key(swag_server_payres:operation_id(), swag_server_payres:api_key()) ->
    false | {true, wapi_auth:context()}.
authorize_api_key(OperationID, ApiKey) ->
    ok = scoper:add_meta(#{api => payres, operation_id => OperationID}),
    wapi_auth:authorize_api_key(OperationID, ApiKey).

-spec handle_request(swag_server_payres:operation_id(), req_data(), swag_server_payres:request_context()) ->
    request_result().
handle_request(OperationID, Req, SwagContext) ->
    wapi_handler:handle_request(OperationID, Req, SwagContext, ?MODULE).

-spec process_request(swag_server_payres:operation_id(), req_data(), handler_context()) ->
    request_result().
process_request('StoreBankCard', Req, Context) ->
    {CardData, AuthData} = process_card_data(Req, Context),
    {ok, {201, [], maps:merge(to_swag(CardData), to_swag(AuthData))}};
process_request('GetBankCard', #{'token' := Token}, _Context) ->
    case decode_token(Token) of
        {ok, Data} ->
            {ok, {200, [], Data}};
        {error, badarg} ->
            {ok, {404, [], undefined}}
    end.

%% Internal functions

process_card_data(#{'BankCard' := Data}, Context) ->
    put_card_data_to_cds(to_thrift(card_data, Data), to_thrift(session_data, Data), Context).

put_card_data_to_cds(CardData, SessionData, Context) ->
    Call = {cds_storage, 'PutCardData', [CardData, SessionData]},
    case service_call(Call, Context) of
        {ok, #'PutCardDataResult'{session_id = SessionID, bank_card = BankCard}} ->
            {{bank_card, BankCard}, {auth_data, SessionID}};
        {exception, Exception} ->
            case Exception of
                #'InvalidCardData'{} ->
                    wapi_handler:throw_result({ok, {400, [],
                        wapi_handler:logic_error(invalidRequest, <<"Card data is invalid">>)
                    }});
                #'KeyringLocked'{} ->
                    % TODO
                    % It's better for the cds to signal woody-level unavailability when the
                    % keyring is locked, isn't it? It could always mention keyring lock as a
                    % reason in a woody error definition.
                    wapi_handler:throw_result({error, wapi_handler:reply_5xx(503)})
            end
    end.

to_thrift(card_data, Data) ->
    {Month, Year} = parse_exp_date(genlib_map:get(<<"expDate">>, Data)),
    CardNumber = genlib:to_binary(genlib_map:get(<<"cardNumber">>, Data)),
    #'CardData'{
        pan  = CardNumber,
        exp_date = #'ExpDate'{
            month = Month,
            year = Year
        },
        cardholder_name = genlib_map:get(<<"cardHolder">>, Data, undefined),
        cvv             = genlib_map:get(<<"cvv">>, Data, undefined)
    };
to_thrift(session_data, Data) ->
    #'SessionData'{
        auth_data = {card_security_code, #'CardSecurityCode'{
            value = genlib_map:get(<<"cvv">>, Data)
        }}
    }.

to_swag({Spec, Data}) when is_atom(Spec) ->
    to_swag(Spec, Data).

to_swag(bank_card, #domain_BankCard{
    'token'          = Token,
    'payment_system' = PaymentSystem,
    'bin'            = Bin,
    'masked_pan'     = MaskedPan
}) ->
    BankCard = genlib_map:compact(#{
        <<"token">>          => Token,
        <<"paymentSystem">>  => genlib:to_binary(PaymentSystem),
        <<"bin">>            => Bin,
        <<"lastDigits">>     => get_last_digits(MaskedPan)
    }),
    BankCard#{<<"token">> => encode_token(BankCard)};

to_swag(auth_data, PaymentSessionID) ->
    #{<<"authData">> => genlib:to_binary(PaymentSessionID)}.

-define(MASKED_PAN_MAX_LENGTH, 4).

get_last_digits(MaskedPan) when byte_size(MaskedPan) > ?MASKED_PAN_MAX_LENGTH ->
    binary:part(MaskedPan, {byte_size(MaskedPan), -?MASKED_PAN_MAX_LENGTH});
get_last_digits(MaskedPan) ->
    MaskedPan.

encode_token(TokenData) ->
    wapi_utils:map_to_base64url(TokenData).

decode_token(Token) ->
    try wapi_utils:base64url_to_map(Token) of
        Data = #{<<"token">> := _} ->
            {ok, maps:with([<<"token">>, <<"paymentSystem">>, <<"bin">>, <<"lastDigits">>],
                Data#{<<"token">> => Token})
            };
        _ ->
            {error, badarg}
    catch
        error:badarg ->
            {error, badarg}
    end.

parse_exp_date(ExpDate) when is_binary(ExpDate) ->
    [Month, Year0] = binary:split(ExpDate, <<"/">>),
    Year = case genlib:to_int(Year0) of
        Y when Y < 100 ->
            2000 + Y;
        Y ->
            Y
    end,
    {genlib:to_int(Month), Year}.

service_call({ServiceName, Function, Args}, #{woody_context := WoodyContext}) ->
    wapi_woody_client:call_service(ServiceName, Function, Args, WoodyContext).
