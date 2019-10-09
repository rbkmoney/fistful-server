-module(p2p_transfer).

-type wallet()   :: ff_wallet:wallet().
-type sender()   :: p2p_instrument:instrument().
-type receiver() :: p2p_instrument:instrument().

-export([get_operation_plan/3]).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-spec get_operation_plan(wallet(), sender(), receiver()) ->
    ok.

get_operation_plan(Wallet, _Sender, _Receiver) ->
    % {ok, PaymentInstitutionID} = ff_party:get_wallet_payment_institution_id(Wallet),
    % {ok, PaymentInstitution} = ff_payment_institution:get(PaymentInstitutionID),

    {ok, IdentityMachine} = ff_identity_machine:get(ff_wallet:identity(Wallet)),
    Identity = ff_identity_machine:identity(IdentityMachine),
    PartyID = ff_identity:party(Identity),
    ContractID = ff_identity:contract(Identity),
    VS = #{party_id => PartyID, cost =>},
    {ok, Terms} = ff_party:get_contract_terms(PartyID, ContractID, VS, ff_time:now()),
    ct:print("Terms: ~p~n", [Terms]),
    #domain_TermSet{
        wallets = #domain_WalletServiceTerms{
            p2p = P2pServiceTerms
        }
    } = Terms,
    % #domain_P2pServiceTerms{
%             operation_plan = OperationPlan
%         } = P2pServiceTerms,
    ct:print("OperationPlan: ~p~n", [P2pServiceTerms]),
    ok.
