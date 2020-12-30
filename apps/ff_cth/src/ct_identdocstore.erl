-module(ct_identdocstore).

-export([rus_domestic_passport/1]).
-export([rus_retiree_insurance_cert/2]).

%%

-include_lib("identdocstore_proto/include/identdocstore_identity_document_storage_thrift.hrl").

-spec rus_domestic_passport(ct_helper:config()) -> {rus_domestic_passport, binary()}.
rus_domestic_passport(C) ->
    Document = {
        russian_domestic_passport,
        #identdocstore_RussianDomesticPassport{
            series = <<"1234">>,
            number = <<"567890">>,
            issuer = <<"Чаржбекистон УВД"/utf8>>,
            issuer_code = <<"012345">>,
            issued_at = <<"2012-12-22T12:42:11Z">>,
            family_name = <<"Котлетка"/utf8>>,
            first_name = <<"С"/utf8>>,
            patronymic = <<"Пюрешкой"/utf8>>,
            birth_date = <<"1972-03-12T00:00:00Z">>,
            birth_place = <<"Чаржбечхала"/utf8>>
        }
    },
    Client = ff_woody_client:new(maps:get('identdocstore', ct_helper:cfg(services, C))),
    WoodyCtx = ct_helper:get_woody_ctx(C),
    Request = {{identdocstore_identity_document_storage_thrift, 'IdentityDocumentStorage'}, 'Put', {Document}},
    case woody_client:call(Request, Client, WoodyCtx) of
        {ok, Token} ->
            {rus_domestic_passport, Token}
    end.

-spec rus_retiree_insurance_cert(_Number :: binary(), ct_helper:config()) -> {rus_retiree_insurance_cert, binary()}.
rus_retiree_insurance_cert(Number, C) ->
    Document = {
        russian_retiree_insurance_certificate,
        #identdocstore_RussianRetireeInsuranceCertificate{
            number = Number
        }
    },
    Client = ff_woody_client:new(maps:get('identdocstore', ct_helper:cfg(services, C))),
    WoodyCtx = ct_helper:get_woody_ctx(C),
    Request = {{identdocstore_identity_document_storage_thrift, 'IdentityDocumentStorage'}, 'Put', {Document}},
    case woody_client:call(Request, Client, WoodyCtx) of
        {ok, Token} ->
            {rus_retiree_insurance_cert, Token}
    end.
