%% TODO
%%    switch to wapi_privdoc_handler when wapi becomes a whole service.
%% Note
%%    It's a bit dirty to call cds directly from a non-pcidss service
%%    even though we get only presentaton data from cds here, not actual card data.

-module(wapi_privdoc_backend).

-include_lib("identdocstore_proto/include/identdocstore_identity_document_storage_thrift.hrl").

-export([get_proof/2]).

%% Types

-type handler_context() :: wapi_handler:context().

%% API

-spec get_proof(binary(), handler_context()) -> map().
get_proof(Token, Context) ->
    {ok, DocData} = wapi_handler_utils:service_call({identdoc_storage, 'Get', {Token}}, Context),
    to_swag(doc_data, {DocData, Token}).

to_swag(doc_data, {{russian_domestic_passport, D}, Token}) ->
    to_swag(doc, {
        #{
            <<"type">> => <<"RUSDomesticPassportData">>,
            <<"series">> => D#'identdocstore_RussianDomesticPassport'.series,
            <<"number">> => D#'identdocstore_RussianDomesticPassport'.number,
            <<"firstName">> => D#'identdocstore_RussianDomesticPassport'.first_name,
            <<"familyName">> => D#'identdocstore_RussianDomesticPassport'.family_name,
            <<"patronymic">> => D#'identdocstore_RussianDomesticPassport'.patronymic
        },
        Token
    });
to_swag(doc_data, {{russian_retiree_insurance_certificate, D}, Token}) ->
    to_swag(doc, {
        #{
            <<"type">> => <<"RUSRetireeInsuranceCertificateData">>,
            <<"number">> => D#'identdocstore_RussianRetireeInsuranceCertificate'.number
        },
        Token
    });
to_swag(doc, {Params, Token}) ->
    Doc = to_swag(raw_doc, {Params, Token}),
    Doc#{<<"token">> => wapi_utils:map_to_base64url(Doc)};
to_swag(raw_doc, {Params = #{<<"type">> := <<"RUSDomesticPassportData">>}, Token}) ->
    #{
        <<"type">> => <<"RUSDomesticPassport">>,
        <<"token">> => Token,
        <<"seriesMasked">> => mask(pass_series, Params),
        <<"numberMasked">> => mask(pass_number, Params),
        <<"fullnameMasked">> => mask(pass_fullname, Params)
    };
to_swag(raw_doc, {Params = #{<<"type">> := <<"RUSRetireeInsuranceCertificateData">>}, Token}) ->
    #{
        <<"type">> => <<"RUSRetireeInsuranceCertificate">>,
        <<"token">> => Token,
        <<"numberMasked">> => mask(retiree_insurance_cert_number, Params)
    }.

-define(PATTERN_DIGIT, [<<"0">>, <<"1">>, <<"2">>, <<"3">>, <<"4">>, <<"5">>, <<"6">>, <<"7">>, <<"8">>, <<"9">>]).

mask(pass_series, #{<<"series">> := V}) ->
    wapi_utils:mask_and_keep(leading, 2, $*, V);
mask(pass_number, #{<<"number">> := V}) ->
    wapi_utils:mask_and_keep(trailing, 1, $*, V);
mask(pass_fullname, Params) ->
    MaskedFamilyName = mask(family_name, Params),
    MaskedFirstName = mask(first_name, Params),
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
    LastPublicSymbols = 1,
    V1 = binary:part(Number, {0, FirstPublicSymbols}),
    Rest1 = binary:part(Number, {0 + FirstPublicSymbols, size(Number) - (0 + FirstPublicSymbols)}),

    V2 = binary:part(Rest1, {size(Rest1), -LastPublicSymbols}),
    Rest2 = binary:part(Rest1, {0, size(Rest1) - LastPublicSymbols}),

    Mask = binary:replace(Rest2, ?PATTERN_DIGIT, <<"*">>, [global]),
    <<V1/binary, Mask/binary, V2/binary>>.
