%%%
%%% Domain config frontend
%%%

-module(ff_domain_config).

-export([object/1]).
-export([object/2]).

%%

-include_lib("dmsl/include/dmsl_domain_config_thrift.hrl").

-type ref()         :: dmsl_domain_config_thrift:'Reference'().
-type object_data() :: _.
-type object_ref()  :: dmsl_domain_thrift:'Reference'().

-spec object(object_ref()) ->
    {ok, object_data()} | {error, notfound}.

-spec object(ref(), object_ref()) ->
    {ok, object_data()} | {error, notfound}.

object(ObjectRef) ->
    object(head(), ObjectRef).

object(Ref, {Type, ObjectRef}) ->
    try dmt_client:checkout_object(Ref, {Type, ObjectRef}) of
        #'VersionedObject'{object = Object} ->
            {Type, {_RecordName, ObjectRef, ObjectData}} = Object,
            {ok, ObjectData}
    catch
        #'ObjectNotFound'{} ->
            {error, notfound}
    end.

head() ->
    {'head', #'Head'{}}.
