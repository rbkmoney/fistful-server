-module(wapi_swagger_validator).

-type param_rule() :: swag_server_wallet_param_validator:param_rule().
-type schema_rule() :: swag_server_wallet_schema_validator:schema_rule().
-type value() :: swag_server_wallet:value().
-type param_context() :: swag_server_wallet_param_validator:context().
-type schema_context() :: swag_server_wallet_schema_validator:context().

-type validate_param_result() ::
    ok | {ok, term()} | pass | error | {error, Error :: term()}.

-type validate_schema_result() ::
    jesse_state:state() | pass | no_return().

-behaviour(swag_server_wallet_custom_validator).

-export([validate_param/3]).
-export([validate_schema/4]).

-spec validate_param(param_rule(), value(), param_context()) -> validate_param_result().
validate_param(_Rule, _Value, _Meta) ->
    pass.

-spec validate_schema(schema_rule(), value(), schema_context(), jesse_state:state()) -> validate_schema_result().
validate_schema(
    {<<"type">>, <<"string">>},
    Value,
    #{
        operation_id := 'CreateDestination',
        definition_name := 'Destination',
        % current_path := [<<"name">>], % check all fields
        msg_type := request
    },
    JesseState
) when is_binary(Value) ->
    case check_destination_name(Value) of
        ok ->
            % pass back to the built-in validator
            pass;
        error ->
            jesse_error:handle_data_invalid(wrong_format, Value, JesseState)
    end;
validate_schema(_Rule, _Value, _Meta, _JesseState) ->
    pass.

check_destination_name(Name) ->
    case re:run(Name, <<"\\d{12,19}">>, [{capture, all, binary}, global]) of
        nomatch -> ok;
        {match, Captured} -> check_luhn(Captured)
    end.

check_luhn([]) ->
    ok;
check_luhn([Captured | Rest]) ->
    case lists:any(fun do_check_luhn/1, Captured) of
        true -> error;
        false -> check_luhn(Rest)
    end.

do_check_luhn(String) ->
    do_check_luhn(String, 0).

do_check_luhn(<<CheckSum>>, Sum) ->
    case Sum * 9 rem 10 of
        M when M =:= CheckSum - $0 ->
            true;
        _M ->
            false
    end;
do_check_luhn(<<N, Rest/binary>>, Sum) when byte_size(Rest) rem 2 =:= 1 ->
    case (N - $0) * 2 of
        M when M >= 10 ->
            do_check_luhn(Rest, Sum + M div 10 + M rem 10);
        M ->
            do_check_luhn(Rest, Sum + M)
    end;
do_check_luhn(<<N, Rest/binary>>, Sum) ->
    do_check_luhn(Rest, Sum + N - $0).
