-module(ct_cardstore).

-export([bank_card/3]).

%%

-include_lib("cds_proto/include/cds_proto_storage_thrift.hrl").

-spec bank_card(binary(), {1..12, 2000..9999}, ct_helper:config()) ->
    #{
        token          := binary(),
        bin            => binary(),
        masked_pan     => binary()
    }.

bank_card(PAN, {MM, YYYY}, C) ->
    CardData = #cds_PutCardData{
        pan      = PAN,
        exp_date = #cds_ExpDate{month = MM, year = YYYY}
    },
    SessionData = #cds_SessionData{
        auth_data = {card_security_code, #cds_CardSecurityCode{value = <<>>}}
    },
    Client = ff_woody_client:new(maps:get('cds', ct_helper:cfg(services, C))),
    WoodyCtx = ct_helper:get_woody_ctx(C),
    Request = {{cds_proto_storage_thrift, 'Storage'}, 'PutCardData', [CardData, SessionData]},
    case woody_client:call(Request, Client, WoodyCtx) of
        {ok, #cds_PutCardDataResult{bank_card = #cds_BankCard{
            token          = Token,
            bin            = BIN,
            last_digits    = Masked
        }}} ->
            #{
                token          => Token,
                bin            => BIN,
                masked_pan     => Masked
            }
    end.
