%%
%% Identificators-related utils

-module(ff_id).

-export([generate_snowflake_id/0]).

%% Types

-type binary_id() :: binary().

-export_type([binary_id/0]).

%% API

-spec generate_snowflake_id() -> binary_id().
generate_snowflake_id() ->
    <<ID:64>> = snowflake:new(),
    genlib_format:format_int_base(ID, 62).
