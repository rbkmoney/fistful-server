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

-module(ff_withdrawal_route_attempt_utils).

-type p_transfer() :: ff_postings_transfer:transfer().
-type limit_check_details() :: ff_withdrawal:limit_check_details().
-type account()  :: ff_account:account().
-type route() :: ff_withdrawal_routing:route().
-type session() :: ff_withdrawal:session().

-type attempt() :: #{
    session => session(),
    p_transfer => p_transfer(),
    limit_checks => [limit_check_details()]
}.

-opaque attempts() :: #{
    attempts := #{route() => attempt()},
    inversed_routes := [route()],
    index := non_neg_integer(),
    current => route()
}.

-export_type([attempts/0]).

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

-spec new_route(route(), attempts()) ->
    attempts().
new_route(Route, undefined) ->
    new_route(Route, init());
new_route(Route, Existing) ->
    #{
        attempts := Attempts,
        inversed_routes := InvRoutes,
        index := Index
    } = Existing,
    Existing#{
        current => Route,
        index => Index + 1,
        inversed_routes => [Route | InvRoutes],
        attempts => Attempts#{Route => #{}}
    }.

-spec next_route([route()], attempts()) -> {ok, route()} | {error, route_not_found}.
next_route(Routes, #{attempts := Existing}) ->
    PendingRoutes =
        lists:filter(
            fun(R) ->
                not maps:is_key(R, Existing)
            end,
            Routes
        ),
    case PendingRoutes of
        [Route | _] ->
            {ok, Route};
        [] ->
            {error, route_not_found}
    end.

-spec get_current_session(attempts()) ->  undefined | session().
get_current_session(Attempts) ->
    Attempt = current(Attempts),
    maps:get(session, Attempt, undefined).

-spec get_current_p_transfer(attempts()) -> undefined | p_transfer().
get_current_p_transfer(Attempts) ->
    Attempt = current(Attempts),
    maps:get(p_transfer, Attempt, undefined).

-spec get_current_limit_checks(attempts()) -> undefined | [limit_check_details()].
get_current_limit_checks(Attempts) ->
    Attempt = current(Attempts),
    maps:get(limit_checks, Attempt, undefined).

-spec update_current_session(session(), attempts()) -> attempts().
update_current_session(Session, Attempts) ->
    Attempt = current(Attempts),
    Updated = Attempt#{
        session => Session
    },
    update_current(Updated, Attempts).

-spec update_current_p_transfer(account(), attempts()) -> attempts().
update_current_p_transfer(Account, Attempts) ->
    Attempt = current(Attempts),
    Updated = Attempt#{
        p_transfer => Account
    },
    update_current(Updated, Attempts).

-spec update_current_limit_checks([limit_check_details()], attempts()) -> attempts().
update_current_limit_checks(LimitChecks, Routes) ->
    Attempt = current(Routes),
    Updated = Attempt#{
        limit_checks => LimitChecks
    },
    update_current(Updated, Routes).

-spec get_sessions(attempts()) -> [session()].
get_sessions(undefined) ->
    [];
get_sessions(#{attempts := Attempts, inversed_routes := InvRoutes}) ->
    lists:foldl(
        fun(ID, Acc) ->
            Route = maps:get(ID, Attempts),
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

-spec get_index(attempts()) -> non_neg_integer().
get_index(#{index := Index}) ->
    Index.

%% Internal

-spec init() -> attempts().
init() ->
    #{
        attempts => #{},
        inversed_routes => [],
        index => 0
    }.

%% @private
current(#{current := Route, attempts := Attempts}) ->
    maps:get(Route, Attempts);
current(_) ->
    #{}.

%% @private
update_current(Attempt, #{current := Route, attempts := Attempts} = R) ->
    R#{
        attempts => Attempts#{
            Route => Attempt
        }
    }.
