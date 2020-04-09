-module(wapi_swagger_validator).

-type param_rule()   :: swag_server_wallet_param_validator:param_rule().
-type schema_rule()  :: swag_server_wallet_schema_validator:schema_rule().
-type value()        :: swag_server_wallet:value().
-type param_meta()   :: swag_server_wallet_param_validator:meta().
-type schema_meta()  :: swag_server_wallet_schema_validator:meta().

-type validate_param_result() ::
    ok | {ok, term()} | pass | error | {error, Error :: term()}.

-type validate_schema_result() ::
    jesse_state:state() | pass | no_return().

-behaviour(swag_server_wallet_custom_validator).

-export([validate_param/3]).
-export([validate_schema/4]).

-spec validate_param(param_rule(), value(), param_meta()) ->
    validate_param_result().
validate_param(_Rule, _Value, _Meta) ->
    pass.

-spec validate_schema(schema_rule(), value(), schema_meta(), jesse_state:state()) ->
    validate_schema_result().

validate_schema(
    {<<"type">>,<<"string">>},
    Value,
    #{
        operation_id := 'CreateDestination',
        definition_name := 'Destination',
        current_path := [<<"name">>],
        msg_type := request
    },
    JesseState
) when is_binary(Value) ->
    case check_destination_name(Value) of
        ok ->
            pass; %pass back to the built-in validator
        error ->
            jesse_error:handle_data_invalid(wrong_format, Value, JesseState)
    end;
validate_schema(_Rule, _Value, _Meta, _JesseState) ->
    pass.

check_destination_name(Name) ->
    case re:run(Name, <<"\\d{12,19}">>, [{capture, none}]) of
        nomatch -> ok;
        match -> error
    end.
