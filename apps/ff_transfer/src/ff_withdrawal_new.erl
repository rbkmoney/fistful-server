%%%
%%% Withdrawal
%%%

-module(ff_withdrawal_new).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-type withdrawal() :: ff_transfer_new:transfer(transfer_params()).
-type transfer_params() :: #{
    wallet_id := wallet_id(),
    destination_id := destination_id()
}.

-type machine() :: ff_transfer_machine_new:st(transfer_params()).
-type events()  :: ff_transfer_machine_new:events(ff_transfer_new:event(transfer_params(), route())).
-type event()   :: ff_transfer_machine_new:event(ff_transfer_new:event(transfer_params(), route())).
-type route()   :: ff_transfer_new:route(#{
    % TODO I'm now sure about this change, it may crash old events. Or not. ))
    provider_id := pos_integer() | id()
}).

-export_type([withdrawal/0]).
-export_type([machine/0]).
-export_type([transfer_params/0]).
-export_type([events/0]).
-export_type([event/0]).
-export_type([route/0]).

%% ff_transfer_machine_new behaviour
-behaviour(ff_transfer_machine_new).
-export([process_transfer/1]).
-export([process_failure/2]).

%% Accessors

-export([wallet_id/1]).
-export([destination_id/1]).
-export([id/1]).
-export([body/1]).
-export([status/1]).
-export([params/1]).
-export([route/1]).
-export([external_id/1]).
-export([transaction/1]).

%% API
-export([create/3]).
-export([get/1]).
-export([get_machine/1]).
-export([events/2]).

%% Event source

-export([maybe_migrate/1]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1, unwrap/2, valid/2]).

%% Internal types

-type id() :: ff_transfer_machine_new:id().
-type body() :: ff_transfer_new:body().
-type wallet_id() :: ff_wallet:id().
-type destination_id() :: ff_destination:id().
-type process_result() :: {ff_transfer_machine_new:action(), [event()]}.

%% Accessors

-spec wallet_id(withdrawal())                   -> wallet_id().
-spec destination_id(withdrawal())              -> destination_id().
-spec id(withdrawal())                          -> ff_transfer_new:id().
-spec body(withdrawal())                        -> body().
-spec status(withdrawal())                      -> ff_transfer_new:status().
-spec params(withdrawal())                      -> transfer_params().
-spec route(withdrawal())                       -> route().
-spec transaction(ff_transfer_new:transfer())   -> ff_transaction_new:transaction().

wallet_id(T)       -> maps:get(wallet_id, ff_transfer_new:params(T)).
destination_id(T)  -> maps:get(destination_id, ff_transfer_new:params(T)).
id(T)              -> ff_transfer_new:id(T).
body(T)            -> ff_transfer_new:body(T).
status(T)          -> ff_transfer_new:status(T).
params(T)          -> ff_transfer_new:params(T).
route(T)           -> ff_transfer_new:route(T).
transaction(T)     -> ff_transfer_new:transaction(T).

-spec external_id(withdrawal()) ->
    id() | undefined.
external_id(T)     -> ff_transfer_new:external_id(T).

%%

-define(NS, 'ff/withdrawal_v3').

-type ctx()    :: ff_ctx:ctx().
-type params() :: #{
    wallet_id      := ff_wallet_machine:id(),
    destination_id := ff_destination:id(),
    body           := ff_transaction:body(),
    external_id    => id()
}.

-spec create(id(), params(), ctx()) ->
    ok |
    {error,
        {source, notfound} |
        {destination, notfound | unauthorized} |
        {provider, notfound} |
        exists |
        _TransferError

    }.

create(ID, Args = #{wallet_id := WalletID, destination_id := DestinationID, body := Body}, Ctx) ->
    do(fun() ->
        Wallet = ff_wallet_machine:wallet(unwrap(wallet, ff_wallet_machine:get(WalletID))),
        WalletAccount = ff_wallet:account(Wallet),
        Destination = ff_destination:get(
            unwrap(destination, ff_destination:get_machine(DestinationID))
        ),
        ok = unwrap(destination, valid(authorized, ff_destination:status(Destination))),
        Terms = unwrap(contract, ff_party:get_contract_terms(Wallet, Body, ff_time:now())),
        valid = unwrap(terms, ff_party:validate_withdrawal_creation(Terms, Body, WalletAccount)),

        Params = #{
            handler     => ?MODULE,
            body        => Body,
            params      => #{
                wallet_id => WalletID,
                destination_id => DestinationID
            },
            session_type => ff_transfer_new:get_session_type(withdrawal),
            external_id => maps:get(external_id, Args, undefined)
        },
        unwrap(ff_transfer_machine_new:create(?NS, ID, Params, Ctx))
    end).

-spec get(machine()) ->
    withdrawal().

get(St) ->
    ff_transfer_machine_new:transfer(St).

-spec get_machine(id()) ->
    {ok, machine()}       |
    {error, notfound}.

get_machine(ID) ->
    ff_transfer_machine_new:get(?NS, ID).

-spec events(id(), machinery:range()) ->
    {ok, events()} |
    {error, notfound}.

events(ID, Range) ->
    ff_transfer_machine_new:events(?NS, ID, Range).

%% ff_transfer_machine_new behaviour

-spec process_transfer(ff_transfer_new:transfer()) ->
    {ok, process_result()} |
    {error, _Reason}.

process_transfer(Withdrawal) ->
    do_process_transfer(ff_transfer_new:activity(Withdrawal), Withdrawal).

-spec process_failure(any(), withdrawal()) ->
    {ok, process_result()} |
    {error, _Reason}.

process_failure(Reason, Withdrawal) ->
    ff_transfer_new:process_failure(Reason, Withdrawal).

%% Internals

do_process_transfer(routing, Withdrawal) ->
    create_route(Withdrawal);
do_process_transfer(transaction_starting, Withdrawal) ->
    create_transaction(Withdrawal);
do_process_transfer(transaction_polling, Withdrawal) ->
    poll_transaction_completion(Withdrawal).

-spec create_route(withdrawal()) ->
    {ok, process_result()} |
    {error, _Reason}.
create_route(Withdrawal) ->
    #{
        wallet_id      := WalletID,
        destination_id := DestinationID
    } = params(Withdrawal),
    Body = body(Withdrawal),
    do(fun () ->
        Wallet = ff_wallet_machine:wallet(unwrap(wallet, ff_wallet_machine:get(WalletID))),
        PaymentInstitutionID = unwrap(ff_party:get_wallet_payment_institution_id(Wallet)),
        PaymentInstitution = unwrap(ff_payment_institution:get(PaymentInstitutionID)),
        DestinationMachine = unwrap(destination, ff_destination:get_machine(DestinationID)),
        Destination = ff_destination:get(DestinationMachine),
        VS = unwrap(collect_varset(Body, Wallet, Destination)),
        Providers = unwrap(ff_payment_institution:compute_withdrawal_providers(PaymentInstitution, VS)),
        ProviderID = unwrap(choose_provider(Providers, VS)),
        {continue, [{route_changed, #{provider_id => ProviderID}}]}
    end).

choose_provider(Providers, VS) ->
    case lists:filter(fun(P) -> validate_withdrawals_terms(P, VS) end, Providers) of
        [ProviderID | _] ->
            {ok, ProviderID};
        [] ->
            {error, route_not_found}
    end.

validate_withdrawals_terms(ID, VS) ->
    Provider = unwrap(ff_payouts_provider:get(ID)),
    case ff_payouts_provider:validate_terms(Provider, VS) of
        {ok, valid} ->
            true;
        {error, _Error} ->
            false
    end.

create_transaction(Withdrawal) ->
    #{
        destination_id := DestinationID,
        wallet_id := WalletID
    } = params(Withdrawal),

    Wallet = ff_wallet_machine:wallet(unwrap(wallet, ff_wallet_machine:get(WalletID))),
    Terms = unwrap(contract, ff_party:get_contract_terms(Wallet, body(Withdrawal), ff_time:now())),
    CashFlowPlan = unwrap(cash_flow_plan, ff_party:get_withdrawal_cash_flow_plan(Terms)),

    TransactionParams = #{
        id            => construct_transaction_id(id(Withdrawal)),
        body          => body(Withdrawal),
        source        => ff_transaction_new:make_ref(wallet, WalletID),
        destination   => ff_transaction_new:make_ref(destination, DestinationID),
        route         => route(Withdrawal),
        transfer_type => ff_transfer_new:transfer_type(Withdrawal),
        cash_flow     => CashFlowPlan,
        session_type  => ff_transaction_new:get_session_type(withdrawal)
    },

    ff_transfer_new:create_transaction(TransactionParams).

poll_transaction_completion(Withdrawal) ->
    ff_transfer_new:poll_transaction_completion(Withdrawal).

-spec maybe_migrate(ff_transfer_new:event() | ff_transfer_new:legacy_event()) ->
    ff_transfer_new:event().
maybe_migrate(Ev) ->
    ff_transfer_new:maybe_migrate(Ev, withdrawal).

collect_varset({_, CurrencyID} = Body, Wallet, Destination) ->
    Currency = #domain_CurrencyRef{symbolic_code = CurrencyID},
    IdentityID = ff_wallet:identity(Wallet),
    do(fun() ->
        IdentityMachine = unwrap(ff_identity_machine:get(IdentityID)),
        Identity = ff_identity_machine:identity(IdentityMachine),
        PartyID = ff_identity:party(Identity),
        PaymentTool = construct_payment_tool(ff_destination:resource(Destination)),
        #{
            currency => Currency,
            cost => ff_cash:encode(Body),
            % TODO it's not fair, because it's PAYOUT not PAYMENT tool.
            payment_tool => PaymentTool,
            party_id => PartyID,
            wallet_id => ff_wallet:id(Wallet),
            payout_method => #domain_PayoutMethodRef{id = wallet_info}
        }
    end).

-spec construct_payment_tool(ff_destination:resource()) ->
    dmsl_domain_thrift:'PaymentTool'().
construct_payment_tool({bank_card, ResourceBankCard}) ->
    {bank_card, #domain_BankCard{
        token           = maps:get(token, ResourceBankCard),
        payment_system  = maps:get(payment_system, ResourceBankCard),
        bin             = maps:get(bin, ResourceBankCard),
        masked_pan      = maps:get(masked_pan, ResourceBankCard)
    }}.

-spec construct_transaction_id(id()) -> id().
construct_transaction_id(ID) ->
    <<"ff/withdrawal/", ID/binary>>.
