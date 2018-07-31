-module(ct_cardstore).

-export([bank_card/3]).

%%

-include_lib("dmsl/include/dmsl_cds_thrift.hrl").

-spec bank_card(binary(), {1..12, 2000..9999}, ct_helper:config()) ->
    #{
        token          := binary(),
        payment_system => atom(),
        bin            => binary(),
        masked_pan     => binary()
    }.

bank_card(PAN, {MM, YYYY}, C) ->
    CardData = #'CardData'{
        pan      = PAN,
        exp_date = #'ExpDate'{month = MM, year = YYYY}
    },
    SessionData = #'SessionData'{
        auth_data = {card_security_code, #'CardSecurityCode'{value = <<>>}}
    },
    Client = ff_woody_client:new(maps:get('cds', ct_helper:cfg(services, C))),
    WoodyCtx = ct_helper:get_woody_ctx(C),
    Request = {{dmsl_cds_thrift, 'Storage'}, 'PutCardData', [CardData, SessionData]},
    case woody_client:call(Request, Client, WoodyCtx) of
        {ok, #'PutCardDataResult'{bank_card = #domain_BankCard{
            token          = Token,
            payment_system = PaymentSystem,
            bin            = BIN,
            masked_pan     = Masked
        }}} ->
            #{
                token          => Token,
                payment_system => PaymentSystem,
                bin            => BIN,
                masked_pan     => Masked
            }
    end.
