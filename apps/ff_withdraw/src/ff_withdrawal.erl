%%%
%%% Withdrawal
%%%

-module(ff_withdrawal).

-type withdrawal() :: #{
    source      := ff_wallet:wallet(),
    destination := ff_destination:destination(),
    trxid       := ff_transaction:id(),
    body        := ff_transfer:body(),
    provider    := ff_provider:provider(),
    transfer    => ff_transfer:transfer()
}.

-export_type([withdrawal/0]).

-export([source/1]).
-export([destination/1]).
-export([trxid/1]).
-export([body/1]).
-export([provider/1]).
-export([transfer/1]).

-export([create/5]).
-export([setup_transfer/1]).
-export([prepare_transfer/1]).
-export([commit_transfer/1]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2]).

%%

source(#{source := V}) -> V.
destination(#{destination := V}) -> V.
trxid(#{trxid := V}) -> V.
body(#{body := V}) -> V.
provider(#{provider := V}) -> V.
transfer(W) -> ff_map:find(transfer, W).

set_transfer(V, W = #{}) -> W#{transfer => V}.
mod_transfer(F, W = #{}) -> W#{transfer := F(transfer(W))}.

%%

create(Source, Destination, TrxID, Body, Provider) ->
    do(fun () ->
        #{
            source      => Source,
            destination => Destination,
            trxid       => TrxID,
            body        => Body,
            provider    => Provider
        }
    end).

setup_transfer(Withdrawal) ->
    do(fun () ->
        Source = source(Withdrawal),
        Destination = ff_destination:wallet(destination(Withdrawal)),
        Transfer = unwrap(ff_transfer:create(
            construct_transfer_id(trxid(Withdrawal)),
            [{Source, Destination, body(Withdrawal)}]
        )),
        set_transfer(Transfer, Withdrawal)
    end).

construct_transfer_id(TrxID) ->
    ff_string:join($/, [TrxID, transfer]).

prepare_transfer(W0) ->
    do(fun () ->
        T0 = transfer(W0),
        T1 = unwrap(transfer, ff_transfer:prepare(T0)),
        W0#{transfer := T1}
    end).

commit_transfer(W0) ->
    do(fun () ->
        T0 = transfer(W0),
        T1 = unwrap(transfer, ff_transfer:commit(T0)),
        W0#{transfer := T1}
    end).

%%

create_provider_withdrawal(W0) ->
    Provider = provider(W0),
    Body     = body(W0),
    PW       = ff_withdrawal_provider:create(Body, Provider),
    {ok, W0#{
        provider_withdrawal => PW,
        status              => provider_withdrawal_created
    }}.

start_provider_withdrawal(W0) ->
    PW0 = provider_withdrawal(W0),
    PW1 = ff_withdrawal_provider:prepare(ff_destination:wallet(destination(W0)), PW0),
    {ok, W0#{
        provider_withdrawal => PW1,
        status              => provider_withdrawal_started
    }}.

await_provider_withdrawal_prepare(W0) ->
    PW = provider_withdrawal(W0),
    case ff_withdrawal_provider:status(PW) of
        prepared ->
            {ok, W0#{
                status => provider_withdrawal_prepared
            }};
        pending ->
            {ok, W0}
    end.

create_withdrawal_session(W0) ->
    Provider    = provider(W0),
    Body        = body(W0),
    Destination = destination(W0),
    Session     = ff_withdrawal_session:start(Provider, Destination, Body),
    {ok, W0#{
        session => Session,
        status  => withdrawal_session_started
    }}.

await_withdrawal_session_complete(W0) ->
    Session = session(W0),
    case ff_withdrawal_session:status(Session) of
        succeeded ->
            {ok, W0#{
                status => withdrawal_session_succeeded
            }};
        pending ->
            {ok, W0}
    end.

commit(W0) ->
    PW0       = provider_withdrawal(W0),
    T0        = transfer(W0),
    {ok, PW1} = ff_withdrawal_provider:commit(PW0),
    {ok, T1}  = ff_transfer:commit(T0),
    {ok, W0#{
        provider_withdrawal => PW1,
        transfer            => T1,
        status              => succeeded
    }}.
