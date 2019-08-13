-module(ff_bin_data).

-include_lib("binbase_proto/include/binbase_binbase_thrift.hrl").
-include_lib("binbase_proto/include/binbase_msgpack_thrift.hrl").

-type token() :: binary().
-type bin_data() :: #{
    token               := token(),
    id                  := bin_data_id(),
    payment_system      := binary(),
    bank_name           => binary(),
    iso_country_code    => atom(),
    card_type           => charge_card | credit | debit | credit_or_debit,
    version             := integer()
}.

-type bin_data_id()     :: %% as stolen from `machinery_msgpack`
    nil                |
    boolean()          |
    integer()          |
    float()            |
    binary()           | %% string
    {binary, binary()} | %% binary
    [bin_data_id()]     |
    #{bin_data_id() => bin_data_id()}.

-export_type([bin_data/0]).
-export_type([bin_data_id/0]).

-export([get/1]).
-export([id/1]).

-spec get(token() | undefined) ->
    {ok, bin_data() | undefined} | {error, not_found}.

get(undefined) ->
    {ok, undefined};
get(Token) ->
    case call_binbase('GetByCardToken', [Token]) of
        {ok, Result} ->
            {ok, decode_result(Token, Result)};
        {exception, #binbase_BinNotFound{}} ->
            {error, not_found}
    end.

-spec id(bin_data()) ->
    bin_data_id().

id(Data) ->
    maps:get(id, Data).

%%

decode_result(Token, #'binbase_ResponseData'{bin_data = Bindata, version = Version}) ->
    #'binbase_BinData'{
        payment_system = PaymentSystem,
        bank_name = BankName,
        iso_country_code = IsoCountryCode,
        card_type = CardType,
        bin_data_id = BinDataID
    } = Bindata,
    genlib_map:compact(#{
        token               => Token,
        id                  => decode_msgpack(BinDataID),
        payment_system      => PaymentSystem,
        bank_name           => BankName,
        iso_country_code    => decode_residence(IsoCountryCode),
        card_type           => decode_card_type(CardType),
        version             => Version
    }).

decode_msgpack({nl, #'binbase_Nil'{}})      -> nil;
decode_msgpack({b,   V}) when is_boolean(V) -> V;
decode_msgpack({i,   V}) when is_integer(V) -> V;
decode_msgpack({flt, V}) when is_float(V)   -> V;
decode_msgpack({str, V}) when is_binary(V)  -> V; % Assuming well-formed UTF-8 bytestring.
decode_msgpack({bin, V}) when is_binary(V)  -> {binary, V};
decode_msgpack({arr, V}) when is_list(V)    -> [decode_msgpack(ListItem) || ListItem <- V];
decode_msgpack({obj, V}) when is_map(V)     ->
    maps:fold(fun(Key, Value, Map) -> Map#{decode_msgpack(Key) => decode_msgpack(Value)} end, #{}, V).

decode_card_type(undefined) ->
    undefined;
decode_card_type(Type) ->
    Type.

decode_residence(undefined) ->
    undefined;
decode_residence(Residence) when is_binary(Residence) ->
    try
        list_to_existing_atom(string:to_lower(binary_to_list(Residence)))
    catch
        error:badarg ->
            throw({decode_residence, invalid_residence})
    end.

call_binbase(Function, Args) ->
    Service = {binbase_binbase_thrift, 'Binbase'},
    ff_woody_client:call(binbase, {Service, Function, Args}).
