-module(ct_cardstore).

-export([bank_card/2]).

%%

-include_lib("cds_proto/include/cds_proto_storage_thrift.hrl").

-spec bank_card(binary(), ct_helper:config()) ->
    #{
        token          := binary(),
        bin            => binary(),
        masked_pan     => binary()
    }.

bank_card(PAN, C) ->
    CardData = #cds_CardData{
        pan      = PAN
    },
    Client = ff_woody_client:new(maps:get('cds', ct_helper:cfg(services, C))),
    WoodyCtx = ct_helper:get_woody_ctx(C),
    Request = {{cds_proto_storage_thrift, 'Storage'}, 'PutCard', [CardData]},
    case woody_client:call(Request, Client, WoodyCtx) of
        {ok, #cds_PutCardResult{bank_card = #cds_BankCard{
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
