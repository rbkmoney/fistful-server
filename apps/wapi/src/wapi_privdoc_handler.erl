-module(wapi_privdoc_handler).

-include_lib("identdocstore_proto/include/identdocstore_identity_document_storage_thrift.hrl").

-behaviour(swag_server_privdoc_logic_handler).
-behaviour(wapi_handler).

%% swag_server_privdoc_logic_handler callbacks
-export([authorize_api_key/3]).
-export([handle_request/4]).

%% wapi_handler callbacks
-export([process_request/4]).

%% helper
%% TODO move it somewhere else
-export([get_proof/2]).

%% Types

-type req_data()        :: wapi_handler:req_data().
-type handler_context() :: wapi_handler:context().
-type request_result()  :: wapi_handler:request_result().
-type operation_id()    :: swag_server_privdoc:operation_id().
-type api_key()         :: swag_server_privdoc:api_key().
-type request_context() :: swag_server_privdoc:request_context().
-type handler_opts()    :: swag_server_privdoc:handler_opts().


%% API

-spec authorize_api_key(operation_id(), api_key(), handler_opts()) ->
    false | {true, wapi_auth:context()}.
authorize_api_key(OperationID, ApiKey, Opts) ->
    ok = scoper:add_meta(#{api => privdoc, operation_id => OperationID}),
    wapi_auth:authorize_api_key(OperationID, ApiKey, Opts).

-spec handle_request(operation_id(), req_data(), request_context(), handler_opts()) ->
    request_result().
handle_request(OperationID, Params, SwagContext, Opts) ->
    wapi_handler:handle_request(OperationID, Params, SwagContext, ?MODULE, Opts).

-spec process_request(operation_id(), req_data(), handler_context(), handler_opts()) ->
    request_result().
process_request('StorePrivateDocument', #{'PrivateDocument' := Params}, Context, _Opts) ->
    wapi_handler_utils:reply_ok(201, process_doc_data(Params, Context)).

process_doc_data(Params, Context) ->
    {ok, Token} = put_doc_data_to_cds(to_thrift(doc_data, Params), Context),
    to_swag(doc, {Params, Token}).

-spec get_proof(binary(), handler_context()) -> map().
get_proof(Token, Context) ->
    {ok, DocData} = service_call({identdoc_storage, 'Get', [Token]}, Context),
    to_swag(doc_data, {DocData, Token}).

to_thrift(doc_data, Params = #{<<"type">> := <<"RUSDomesticPassportData">>}) ->
    {russian_domestic_passport, #'identdocstore_RussianDomesticPassport'{
        series      = maps:get(<<"series">>, Params),
        number      = maps:get(<<"number">>, Params),
        issuer      = maps:get(<<"issuer">>, Params),
        issuer_code = maps:get(<<"issuerCode">>, Params),
        issued_at   = maps:get(<<"issuedAt">>, Params),
        family_name = maps:get(<<"familyName">>, Params),
        first_name  = maps:get(<<"firstName">>, Params),
        patronymic  = maps:get(<<"patronymic">>, Params, undefined),
        birth_date  = maps:get(<<"birthDate">>, Params),
        birth_place = maps:get(<<"birthPlace">>, Params)
      }};
to_thrift(doc_data, Params = #{<<"type">> := <<"RUSRetireeInsuranceCertificateData">>}) ->
    {russian_retiree_insurance_certificate, #'identdocstore_RussianRetireeInsuranceCertificate'{
        number = maps:get(<<"number">>, Params)
    }}.

to_swag(doc, {Params, Token}) ->
    Doc = to_swag(raw_doc, {Params, Token}),
    Doc#{<<"token">> => wapi_utils:map_to_base64url(Doc)};
to_swag(raw_doc, {Params = #{<<"type">> := <<"RUSDomesticPassportData">>}, Token}) ->
    #{
        <<"type">>           => <<"RUSDomesticPassport">>,
        <<"token">>          => Token,
        <<"seriesMasked">>   => mask(pass_series, Params),
        <<"numberMasked">>   => mask(pass_number, Params),
        <<"fullnameMasked">> => mask(pass_fullname, Params)
    };
to_swag(raw_doc, {Params = #{<<"type">> := <<"RUSRetireeInsuranceCertificateData">>}, Token}) ->
    #{
        <<"type">>           => <<"RUSRetireeInsuranceCertificate">>,
        <<"token">>          => Token,
        <<"numberMasked">>   => mask(retiree_insurance_cert_number, Params)
     };
to_swag(doc_data, {{russian_domestic_passport, D}, Token}) ->
    to_swag(doc, {
        #{
            <<"type">>       => <<"RUSDomesticPassportData">>,
            <<"series">>     => D#'identdocstore_RussianDomesticPassport'.series,
            <<"number">>     => D#'identdocstore_RussianDomesticPassport'.number,
            <<"firstName">>  => D#'identdocstore_RussianDomesticPassport'.first_name,
            <<"familyName">> => D#'identdocstore_RussianDomesticPassport'.family_name,
            <<"patronymic">> => D#'identdocstore_RussianDomesticPassport'.patronymic
        },
        Token
    });
to_swag(doc_data, {{russian_retiree_insurance_certificate, D}, Token}) ->
    to_swag(doc, {
        #{
            <<"type">>       => <<"RUSRetireeInsuranceCertificateData">>,
            <<"number">>     => D#'identdocstore_RussianRetireeInsuranceCertificate'.number
        },
        Token
    }).

put_doc_data_to_cds(IdentityDoc, Context) ->
    service_call({identdoc_storage, 'Put', [IdentityDoc]}, Context).

service_call({ServiceName, Function, Args}, #{woody_context := WoodyContext}) ->
    wapi_woody_client:call_service(ServiceName, Function, Args, WoodyContext).

-define(PATTERN_DIGIT, [<<"0">>, <<"1">>, <<"2">>, <<"3">>, <<"4">>, <<"5">>, <<"6">>, <<"7">>, <<"8">>, <<"9">>]).

mask(pass_series, #{<<"series">> := V}) ->
    wapi_utils:mask_and_keep(leading, 2, $*, V);
mask(pass_number, #{<<"number">> := V}) ->
    wapi_utils:mask_and_keep(trailing, 1, $*, V);
mask(pass_fullname, Params) ->
    MaskedFamilyName = mask(family_name, Params),
    MaskedFirstName  = mask(first_name, Params),
    MaskedPatronymic = mask(patronymic, Params),
    <<MaskedFamilyName/binary, " ", MaskedFirstName/binary, MaskedPatronymic/binary>>;
mask(family_name, #{<<"familyName">> := V}) ->
    wapi_utils:mask_and_keep(leading, 1, $*, V);
mask(first_name, #{<<"firstName">> := V}) ->
    <<(unicode:characters_to_binary(string:left(unicode:characters_to_list(V), 1)))/binary, "."/utf8>>;
mask(patronymic, #{<<"patronymic">> := V}) ->
    <<(unicode:characters_to_binary(string:left(unicode:characters_to_list(V), 1)))/binary, "."/utf8>>;
mask(patronymic, _) ->
    <<>>;
%% TODO rewrite this ugly shit
mask(retiree_insurance_cert_number, #{<<"number">> := Number}) ->
    FirstPublicSymbols = 2,
    LastPublicSymbols  = 1,
    V1    = binary:part(Number, {0                     , FirstPublicSymbols}),
    Rest1 = binary:part(Number, {0 + FirstPublicSymbols, size(Number) - (0 + FirstPublicSymbols)}),

    V2    = binary:part(Rest1, {size(Rest1)                   , -LastPublicSymbols}),
    Rest2 = binary:part(Rest1, {0, size(Rest1) - LastPublicSymbols}),

    Mask = binary:replace(Rest2, ?PATTERN_DIGIT, <<"*">>, [global]),
    <<V1/binary, Mask/binary, V2/binary>>.
