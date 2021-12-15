%%%
%%% Domain config helpers
%%%

-module(ct_domain_config).

-export([head/0]).

-export([all/1]).
-export([checkout_object/2]).
-export([commit/2]).
-export([insert/1]).
-export([update/1]).
-export([upsert/1]).
-export([remove/1]).
-export([reset/1]).
-export([cleanup/0]).
-export([bump_revision/0]).

%%

-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").

-type revision() :: dmt_client:version().
-type object() :: dmsl_domain_thrift:'DomainObject'().
-type object_ref() :: dmt_client:object_ref().

-spec head() -> revision().
head() ->
    dmt_client:get_last_version().

-spec all(revision()) -> dmsl_domain_thrift:'Domain'().
all(Revision) ->
    #'Snapshot'{domain = Domain} = dmt_client:checkout(Revision),
    Domain.

-spec checkout_object(revision(), object_ref()) -> object() | no_return().
checkout_object(Revision, ObjectRef) ->
    dmt_client:checkout_object(Revision, ObjectRef).

-spec commit(revision(), dmt_client:commit()) -> revision() | no_return().
commit(Revision, Commit) ->
    dmt_client:commit(Revision, Commit).

-spec insert(object() | [object()]) -> revision() | no_return().
insert(ObjectOrMany) ->
    dmt_client:insert(ObjectOrMany).

-spec update(object() | [object()]) -> revision() | no_return().
update(NewObjectOrMany) ->
    dmt_client:update(NewObjectOrMany).

-spec upsert(object() | [object()]) -> revision() | no_return().
upsert(NewObjectOrMany) ->
    upsert(latest, NewObjectOrMany).

-spec upsert(revision(), object() | [object()]) -> revision() | no_return().
upsert(Revision, NewObjectOrMany) ->
    dmt_client:upsert(Revision, NewObjectOrMany).

-spec remove(object() | [object()]) -> revision() | no_return().
remove(ObjectOrMany) ->
    dmt_client:remove(ObjectOrMany).

-spec reset(revision()) -> revision() | no_return().
reset(Revision) ->
    upsert(maps:values(all(Revision))).

-spec cleanup() -> revision() | no_return().
cleanup() ->
    #'Snapshot'{domain = Domain} = dmt_client:checkout(latest),
    remove(maps:values(Domain)).

-spec bump_revision() -> revision() | no_return().
bump_revision() ->
    dmt_client:commit(#'Commit'{ops = []}).
