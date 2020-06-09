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

-module(ff_withdrawal_attempt_utils).

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

-type attempt() :: #{
    session => session(),
    p_transfer => p_transfer(),
    limit_checks => [limit_check_details()]
}.

-opaque attempts() :: #{
    attempts := #{provider_id() => attempt()},
    inversed_providers := [provider_id()],
    index := non_neg_integer(),
    current => provider_id()
}.

-export_type([attempts/0]).

%% API
-export([init/0]).
-export([new/2]).
-export([next_route/2]).
-export([get_current_session/1]).
-export([get_current_p_transfer/1]).
-export([get_current_limit_checks/1]).
-export([update_current_session/2]).
-export([update_current_p_transfer/2]).
-export([update_current_limit_checks/2]).

-export([get_sessions/1]).
-export([get_index/1]).

-spec init() -> attempts().
init() ->
    #{
        attempts => #{},
        inversed_providers => [],
        index => 0
    }.

-spec new(provider_id(), attempts()) ->
    attempts().
new(PrID, Existing) ->
    #{
        attempts := Attempts,
        inversed_providers := InvProviders,
        index := Index
    } = Existing,
    Existing#{
        current => PrID,
        index => Index + 1,
        inversed_providers => [PrID | InvProviders],
        attempts => Attempts#{PrID => #{}}
    }.

-spec next_route([provider_id()], attempts()) -> {ok, route()} | {error, route_not_found}.
next_route(Providers, #{attempts := Existing}) ->
    PendingProviders =
        lists:filter(
            fun(ID) ->
                not maps:is_key(ID, Existing)
            end,
            Providers
        ),
    case PendingProviders of
        [ProviderID | _] ->
            {ok, #{provider_id => ProviderID}};
        [] ->
            {error, route_not_found}
    end.

-spec get_current_session(attempts()) ->  undefined | session().
get_current_session(Attempts) ->
    Attempt = get_current_attempt(Attempts),
    maps:get(session, Attempt, undefined).

-spec get_current_p_transfer(attempts()) -> undefined | p_transfer().
get_current_p_transfer(Attempts) ->
    Attempt = get_current_attempt(Attempts),
    maps:get(p_transfer, Attempt, undefined).

-spec get_current_limit_checks(attempts()) -> undefined | [limit_check_details()].
get_current_limit_checks(Attempts) ->
    Attempt = get_current_attempt(Attempts),
    maps:get(limit_checks, Attempt, undefined).

-spec update_current_session(session(), attempts()) -> attempts().
update_current_session(Session, Attempts) ->
    Attempt = get_current_attempt(Attempts),
    Updated = Attempt#{
        session => Session
    },
    update_current_attempt(Updated, Attempts).

-spec update_current_p_transfer(account(), attempts()) -> attempts().
update_current_p_transfer(Account, Attempts) ->
    Attempt = get_current_attempt(Attempts),
    Updated = Attempt#{
        p_transfer => Account
    },
    update_current_attempt(Updated, Attempts).

-spec update_current_limit_checks([limit_check_details()], attempts()) -> attempts().
update_current_limit_checks(LimitChecks, Routes) ->
    Attempt = get_current_attempt(Routes),
    Updated = Attempt#{
        limit_checks => LimitChecks
    },
    update_current_attempt(Updated, Routes).

-spec get_sessions(attempts()) -> [session()].
get_sessions(#{attempts := Attempts, inversed_providers := InvProviders}) ->
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
        InvProviders
    ).

-spec get_index(attempts()) -> non_neg_integer().
get_index(#{index := Index}) ->
    Index.

%% Internal

%% @private
get_current_attempt(#{current := PrID, attempts := Routes}) ->
    maps:get(PrID, Routes);
get_current_attempt(_) ->
    #{}.

%% @private
update_current_attempt(Route, #{current := PrID, attempts := Routes} = R) ->
    R#{
        attempts => Routes#{
            PrID => Route
        }
    }.
