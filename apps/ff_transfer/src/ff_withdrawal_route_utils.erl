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
-type withdrawal_state() :: ff_withdrawal:withdrawal_state().
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

-spec new_route(provider_id(), withdrawal_state()) ->
    withdrawal_state().
new_route(PrID, Withdrawal) ->
    #{
        routes := Routes,
        inversed_routes := InvRoutes,
        index := Index
    } = ExRoutes = routes(Withdrawal),
    UpdRoutes = ExRoutes#{
        current => PrID,
        index => Index + 1,
        inversed_routes => [PrID | InvRoutes],
        routes => Routes#{PrID => #{}}
    },
    ff_withdrawal:update_routes(UpdRoutes, Withdrawal).

-spec next_route([provider_id()], withdrawal_state()) -> {ok, route()} | {error, route_not_found}.
next_route(Providers, Withdraval) ->
    #{inversed_routes := ExRoutes} = routes(Withdraval),
    PendingProviders =
        lists:filter(
            fun(ID) ->
                not lists:member(ID, ExRoutes)
            end,
            Providers
        ),
    case PendingProviders of
        [ProviderID | _] ->
            {ok, #{provider_id => ProviderID}};
        [] ->
            {error, route_not_found}
    end.

-spec get_current_session(withdrawal_state()) ->  undefined | session().
get_current_session(Withdrawal) ->
    Route = get_current_route(Withdrawal),
    maps:get(session, Route, undefined).

-spec get_current_p_transfer(withdrawal_state()) -> undefined | p_transfer().
get_current_p_transfer(Withdrawal) ->
    Route = get_current_route(Withdrawal),
    maps:get(p_transfer, Route, undefined).

-spec get_current_limit_checks(withdrawal_state()) -> undefined | [limit_check_details()].
get_current_limit_checks(Withdrawal) ->
    Route = get_current_route(Withdrawal),
    maps:get(limit_checks, Route, undefined).

-spec update_current_session(session(), withdrawal_state()) -> withdrawal_state().
update_current_session(Session, Withdrawal) ->
    Route = get_current_route(Withdrawal),
    UpRoute = Route#{
        session => Session
    },
    update_current_route(UpRoute, Withdrawal).

-spec update_current_p_transfer(account(), withdrawal_state()) -> withdrawal_state().
update_current_p_transfer(Account, Withdrawal) ->
    Route = get_current_route(Withdrawal),
    UpRoute = Route#{
        p_transfer => Account
    },
    update_current_route(UpRoute, Withdrawal).

-spec update_current_limit_checks([limit_check_details()], withdrawal_state()) -> withdrawal_state().
update_current_limit_checks(LimitChecks, Withdrawal) ->
    Route = get_current_route(Withdrawal),
    UpRoute = Route#{
        limit_checks => LimitChecks
    },
    update_current_route(UpRoute, Withdrawal).

-spec get_sessions(withdrawal_state()) -> [session()].
get_sessions(Withdrawal) ->
    #{
        routes := Routes,
        inversed_routes := InvRoutes
    } = routes(Withdrawal),
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

-spec get_index(withdrawal_state()) -> non_neg_integer().
get_index(Withdrawal) ->
    #{index := Index} = routes(Withdrawal),
    Index.

%% Internal

%% @private
get_current_route(Withdrawal) ->
    #{
        current := PrID,
        routes := Routes
    } = routes(Withdrawal),
    maps:get(PrID, Routes).

%% @private
update_current_route(Route, Withdrawal) ->
    #{
        current := PrID,
        routes := Routes
    } = R = routes(Withdrawal),
    UpR = R#{
        routes => Routes#{
            PrID => Route
        }
    },
    ff_withdrawal:update_routes(UpR, Withdrawal).

%% @private
routes(Withdrawal) ->
    case ff_withdrawal:routes(Withdrawal) of
        undefined ->
            init_routes();
        Routes ->
            Routes
    end.

init_routes() ->
    #{
        routes => #{},
        inversed_routes => [],
        index => 0
    }.