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

-module(ff_withdrawal_session_utils).

-type withdrawal_state() :: ff_withdrawal:withdrawal_state().
-type id() :: binary().
-type provider_id() :: pos_integer() | id().

-type result() :: ff_withdrawal_session:session_result().
-type session() :: #{
    id := id(),
    provider_id := provider_id(),
    result => result()
}.

-opaque index() :: #{
    sessions := #{id() => session()},
    inversed_order := [id()],
    current => id()
}.

-export_type([index/0]).

%% API
-export([create_session/3]).
-export([get_current_session/1]).
-export([get_session/2]).
-export([get_session_by_provider_id/2]).
-export([get_sessions/1]).
-export([update_session/2]).

-spec create_session(id(), provider_id(), withdrawal_state()) -> withdrawal_state().
create_session(SessionID, ProviderID, Withdrawal) ->
    #{
        sessions := Sessions,
        inversed_order := Order
    } = index(Withdrawal),
    Session = #{
        id => SessionID,
        provider_id => ProviderID
    },
    NewIndex = #{
        sessions => maps:put(SessionID, Session, Sessions),
        inversed_order => [SessionID | Order],
        current => SessionID
    },
    Withdrawal#{sessions => NewIndex}.

-spec get_current_session(withdrawal_state()) -> session() | undefined.
get_current_session(Withdrawal) ->
    #{sessions := S} = Index = index(Withdrawal),
    case maps:get(current, Index, undefined) of
        undefined ->
            undefined;
        ID ->
            maps:get(ID, S)
    end.

-spec get_session(id(), withdrawal_state()) -> session().
get_session(SessionID, Withdrawal) ->
    #{sessions := Sessions} = index(Withdrawal),
    maps:get(SessionID, Sessions).

-spec get_session_by_provider_id(provider_id(), withdrawal_state()) -> undefined | session().
get_session_by_provider_id(ProviderID, Withdrawal) ->
    #{sessions := Sessions} = index(Withdrawal),
    Iter = maps:iterator(Sessions),
    lookup_session(ProviderID, maps:next(Iter)).

-spec update_session(session(), withdrawal_state()) -> withdrawal_state().
update_session(Session, Withdrawal) ->
    #{id := SessionID} = Session,
    #{sessions := Sessions} = Index = index(Withdrawal),
    Withdrawal#{
        sessions => Index#{
            sessions => Sessions#{SessionID => Session}
        }
    }.

-spec get_sessions(withdrawal_state()) -> [session()].
get_sessions(Withdrawal) ->
    #{sessions := Sessions, inversed_order := Order} = index(Withdrawal),
    [maps:get(ID, Sessions) || ID <- lists:reverse(Order)].

%% Internal

%% @private
index(Withdrawal) ->
    case maps:get(sessions, Withdrawal, undefined) of
        undefined ->
            #{
                sessions => #{},
                inversed_order => []
            };
        Map when is_map(Map) ->
            Map
    end.

%% @private
lookup_session(_PrID, none) ->
    undefined;
lookup_session(PrID, {_, S = #{provider_id := PrID}, _}) ->
    S;
lookup_session(PrID, {_, _, I}) ->
    lookup_session(PrID, maps:next(I)).
