%%%
%%% Copyright 2020 RBKmoney
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%

-module(ff_withdrawal_route_utils).

-type id() :: binary().
-type provider_id() :: pos_integer() | id().
-type p_transfer() :: ff_postings_transfer:transfer().
-type limit_check_details() :: ff_withdrawal:limit_check_details().
-type account()  :: ff_account:account().
-type route() :: ff_withdrawal:route().
-type result() :: ff_withdrawal_session:session_result().
-type session() :: #{
    id := id(),
    result => result()
}.

-type route_data() :: #{
    session => session(),
    p_transfer => p_transfer(),
    limit_checks => [limit_check_details()]
}.

-opaque routes() :: #{
    routes := #{provider_id() => route_data()},
    inversed_routes := [provider_id()],
    index := non_neg_integer(),
    current => provider_id()
}.

-export_type([routes/0]).

%% API
-export([init_routes/0]).
-export([new_route/2]).
-export([next_route/2]).
-export([get_current_session/1]).
-export([get_current_p_transfer/1]).
-export([get_current_limit_checks/1]).
-export([update_current_session/2]).
-export([update_current_p_transfer/2]).
-export([update_current_limit_checks/2]).

-export([get_sessions/1]).
-export([get_index/1]).

-spec init_routes() -> routes().
init_routes() ->
    #{
        routes => #{},
        inversed_routes => [],
        index => 0
    }.

-spec new_route(provider_id(), routes()) ->
    routes().
new_route(PrID, ExRoutes) ->
    #{
        routes := Routes,
        inversed_routes := InvRoutes,
        index := Index
    } = ExRoutes,
    ExRoutes#{
        current => PrID,
        index => Index + 1,
        inversed_routes => [PrID | InvRoutes],
        routes => Routes#{PrID => #{}}
    }.

-spec next_route([provider_id()], routes()) -> {ok, route()} | {error, route_not_found}.
next_route(Providers, #{routes := ExRoutes}) ->
    PendingProviders =
        lists:filter(
            fun(ID) ->
                not maps:is_key(ID, ExRoutes)
            end,
            Providers
        ),
    case PendingProviders of
        [ProviderID | _] ->
            {ok, #{provider_id => ProviderID}};
        [] ->
            {error, route_not_found}
    end.

-spec get_current_session(routes()) ->  undefined | session().
get_current_session(Routes) ->
    Route = get_current_route(Routes),
    maps:get(session, Route, undefined).

-spec get_current_p_transfer(routes()) -> undefined | p_transfer().
get_current_p_transfer(Routes) ->
    Route = get_current_route(Routes),
    maps:get(p_transfer, Route, undefined).

-spec get_current_limit_checks(routes()) -> undefined | [limit_check_details()].
get_current_limit_checks(Routes) ->
    Route = get_current_route(Routes),
    maps:get(limit_checks, Route, undefined).

-spec update_current_session(session(), routes()) -> routes().
update_current_session(Session, Routes) ->
    Route = get_current_route(Routes),
    UpRoute = Route#{
        session => Session
    },
    update_current_route(UpRoute, Routes).

-spec update_current_p_transfer(account(), routes()) -> routes().
update_current_p_transfer(Account, Routes) ->
    Route = get_current_route(Routes),
    UpRoute = Route#{
        p_transfer => Account
    },
    update_current_route(UpRoute, Routes).

-spec update_current_limit_checks([limit_check_details()], routes()) -> routes().
update_current_limit_checks(LimitChecks, Routes) ->
    Route = get_current_route(Routes),
    UpRoute = Route#{
        limit_checks => LimitChecks
    },
    update_current_route(UpRoute, Routes).

-spec get_sessions(routes()) -> [session()].
get_sessions(#{routes := Routes, inversed_routes := InvRoutes}) ->
    lists:foldl(
        fun(ID, Acc) ->
            Route = maps:get(ID, Routes),
            case maps:get(session, Route, undefined) of
                undefined ->
                    Acc;
                Session ->
                    [Session | Acc]
            end
        end,
        [],
        InvRoutes
    ).

-spec get_index(routes()) -> non_neg_integer().
get_index(Routes) ->
    maps:get(index, Routes).

%% Internal

%% @private
get_current_route(#{current := PrID, routes := Routes}) ->
    maps:get(PrID, Routes);
get_current_route(_) ->
    #{}.

%% @private
update_current_route(Route, #{current := PrID, routes := Routes} = R) ->
    R#{
        routes => Routes#{
            PrID => Route
        }
    }.
