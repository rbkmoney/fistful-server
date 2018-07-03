-module(ct_identdocstore).

-export([rus_domestic_passport/1]).
-export([rus_retiree_insurance_cert/2]).

%%

-include_lib("identdocstore_proto/include/identdocstore_identity_document_storage_thrift.hrl").

rus_domestic_passport(C) ->
    Document = {
        russian_domestic_passport,
        #identdocstore_RussianDomesticPassport{
            series      = <<"1234">>,
            number      = <<"567890">>,
            issuer      = <<"Чаржбекистон УВД">>,
            issuer_code = <<"012345">>,
            issued_at   = <<"2012-12-22T12:42:11Z">>,
            family_name = <<"Котлетка">>,
            first_name  = <<"С">>,
            patronymic  = <<"Пюрешкой">>,
            birth_date  = <<"1972-03-12T00:00:00Z">>,
            birth_place = <<"Чаржбечхала">>
        }
    },
    Client = maps:get('identdocstore', ct_helper:cfg(services, C)),
    WoodyCtx = ct_helper:get_woody_ctx(C),
    Request = {{identdocstore_identity_document_storage_thrift, 'IdentityDocumentStorage'}, 'Put', [Document]},
    case woody_client:call(Request, Client, WoodyCtx) of
        {ok, Token} ->
            {rus_domestic_passport, Token}
    end.

rus_retiree_insurance_cert(Number, C) ->
    Document = {
        russian_retiree_insurance_certificate,
        #identdocstore_RussianRetireeInsuranceCertificate{
            number = Number
        }
    },
    Client = maps:get('identdocstore', ct_helper:cfg(services, C)),
    WoodyCtx = ct_helper:get_woody_ctx(C),
    Request = {{identdocstore_identity_document_storage_thrift, 'IdentityDocumentStorage'}, 'Put', [Document]},
    case woody_client:call(Request, Client, WoodyCtx) of
        {ok, Token} ->
            {rus_retiree_insurance_cert, Token}
    end.
