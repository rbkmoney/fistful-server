-module(ff_utils).

-define(CTX_NS, <<"com.rbkmoney.wapi">>).

-export([get_owner/1]).

-spec get_owner(map()) ->
    binary() | undefined.
get_owner(#{?CTX_NS := Context}) ->
    genlib_map:get(<<"owner">>, Context).
