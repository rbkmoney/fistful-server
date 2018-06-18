%%%
%%% Managed party
%%%
%%% TODOs
%%%
%%%  - We expect party to exist, which is certainly not the general case.
%%%


-module(ff_party).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-type id()          :: dmsl_domain_thrift:'PartyID'().
-type contract()    :: dmsl_domain_thrift:'ContractID'().
-type wallet()      :: dmsl_domain_thrift:'WalletID'().

-export_type([id/0]).
-export_type([contract/0]).
-export_type([wallet/0]).

-export([is_accessible/1]).
-export([get_wallet/2]).
-export([get_wallet_account/2]).
-export([is_wallet_accessible/2]).
-export([create_contract/2]).
-export([create_wallet/3]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%

-spec is_accessible(id()) ->
    {ok, accessible} |
    {error, {inaccessible, suspended | blocked}}.

is_accessible(ID) ->
    case do_get_party(ID) of
        #domain_Party{blocking = {blocked, _}} ->
            {error, {inaccessible, blocked}};
        #domain_Party{suspension = {suspended, _}} ->
            {error, {inaccessible, suspended}};
        #domain_Party{} ->
            {ok, accessible}
    end.

-spec get_wallet(id(), wallet()) ->
    {ok, _Wallet} |
    {error, notfound}.

get_wallet(ID, WalletID) ->
    do_get_wallet(ID, WalletID).

-spec get_wallet_account(id(), wallet()) ->
    {ok, ff_transaction:account()} |
    {error, notfound}.

get_wallet_account(ID, WalletID) ->
    do(fun () ->
        #domain_Wallet{
            account = #domain_WalletAccount{
                settlement = AccountID
            }
        } = unwrap(get_wallet(ID, WalletID)),
        AccountID
    end).

-spec is_wallet_accessible(id(), wallet()) ->
    {ok, accessible} |
    {error,
        notfound |
        {inaccessible, suspended | blocked}
    }.

is_wallet_accessible(ID, WalletID) ->
    case do_get_wallet(ID, WalletID) of
        {ok, #domain_Wallet{blocking = {blocked, _}}} ->
            {error, {inaccessible, blocked}};
        {ok, #domain_Wallet{suspension = {suspended, _}}} ->
            {error, {inaccessible, suspended}};
        {ok, #domain_Wallet{}} ->
            {ok, accessible};
        {error, notfound} ->
            {error, notfound}
    end.

%%

-type contract_prototype() :: #{
    payinst           := _PaymentInstitutionRef,
    contract_template := _ContractTemplateRef
}.

-spec create_contract(id(), contract_prototype()) ->
    {ok, contract()} |
    {error, invalid}.

create_contract(ID, Prototype) ->
    do(fun () ->
        ContractID = generate_contract_id(),
        Changeset  = construct_contract_changeset(ContractID, Prototype),
        Claim      = unwrap(do_create_claim(ID, Changeset)),
        accepted   = do_accept_claim(ID, Claim),
        ContractID
    end).

generate_contract_id() ->
    uuid:get_v4().

%%

-type wallet_prototype() :: #{
    name     := binary(),
    currency := ff_currency:id()
}.

-spec create_wallet(id(), contract(), wallet_prototype()) ->
    {ok, wallet()} |
    {error, invalid}.

create_wallet(ID, ContractID, Prototype) ->
    do(fun () ->
        WalletID  = generate_wallet_id(),
        Changeset = construct_wallet_changeset(ContractID, WalletID, Prototype),
        Claim     = unwrap(do_create_claim(ID, Changeset)),
        accepted  = do_accept_claim(ID, Claim),
        WalletID
    end).

generate_wallet_id() ->
    uuid:get_v4().


%% Party management client

do_get_party(ID) ->
    case call('Get', [construct_userinfo(), ID]) of
        {ok, #domain_Party{} = Party} ->
            Party;
        {exception, Unexpected} ->
            error(Unexpected)
    end.

do_get_wallet(ID, WalletID) ->
    case call('GetWallet', [construct_userinfo(), ID, WalletID]) of
        {ok, #domain_Wallet{} = Wallet} ->
            {ok, Wallet};
        {exception, #payproc_WalletNotFound{}} ->
            {error, notfound};
        {exception, Unexpected} ->
            error(Unexpected)
    end.

do_create_claim(ID, Changeset) ->
    case call('CreateClaim', [construct_userinfo(), ID, Changeset]) of
        {ok, Claim} ->
            {ok, Claim};
        {exception, #payproc_InvalidChangeset{reason = _Reason}} ->
            {error, invalid};
        {exception, Unexpected} ->
            error(Unexpected)
    end.

do_accept_claim(ID, Claim) ->
    % TODO
    %  - We assume here that there's only one actor (identity machine) acting in
    %    such a way which may cause conflicts.
    ClaimID  = Claim#payproc_Claim.id,
    Revision = Claim#payproc_Claim.revision,
    case call('AcceptClaim', [construct_userinfo(), ID, ClaimID, Revision]) of
        {ok, ok} ->
            accepted;
        {exception, #payproc_InvalidClaimStatus{status = {accepted, _}}} ->
            accepted;
        {exception, Unexpected} ->
            error(Unexpected)
    end.

%%

-define(contractor_mod(ID, Mod),
    {contractor_modification,
        #payproc_ContractorModificationUnit{id = ID, modification = Mod}}
).

-define(contract_mod(ID, Mod),
    {contract_modification,
        #payproc_ContractModificationUnit{id = ID, modification = Mod}}
).

-define(wallet_mod(ID, Mod),
    {wallet_modification,
        #payproc_WalletModificationUnit{id = ID, modification = Mod}}
).

construct_contract_changeset(ContractID, #{
    payinst           := PayInstRef,
    contract_template := ContractTemplateRef
}) ->
    [
        ?contractor_mod(
            ContractID,
            {creation, {private_entity, {russian_private_entity,
                #domain_RussianPrivateEntity{
                    % TODO
                    first_name   = <<>>,
                    second_name  = <<>>,
                    middle_name  = <<>>,
                    contact_info = #domain_ContactInfo{}
                }
            }}}
        ),
        ?contract_mod(
            ContractID,
            {creation, #payproc_ContractParams{
                contractor_id       = ContractID,
                payment_institution = PayInstRef,
                template            = ContractTemplateRef
            }}
        )
    ].

construct_wallet_changeset(ContractID, WalletID, #{
    name     := Name,
    currency := Currency
}) ->
    [
        ?wallet_mod(
            WalletID,
            {creation, #payproc_WalletParams{
                name        = Name,
                contract_id = ContractID
            }}
        ),
        ?wallet_mod(
            WalletID,
            {account_creation, #payproc_WalletAccountParams{
                currency    = #domain_CurrencyRef{symbolic_code = Currency}
            }}
        )
    ].

construct_userinfo() ->
    #{id := ID, realm := Realm} = ff_woody_ctx:get_user_identity(),
    #payproc_UserInfo{id = ID, type = construct_usertype(Realm)}.

construct_usertype(<<"external">>) ->
    {external_user, #payproc_ExternalUser{}};
construct_usertype(<<"internal">>) ->
    {internal_user, #payproc_InternalUser{}}.

%% Woody stuff

call(Function, Args) ->
    % TODO
    %  - Ideally, we should provide `Client` here explicitly.
    Service = {dmsl_payment_processing_thrift, 'PartyManagement'},
    ff_woody_client:call(partymgmt, {Service, Function, Args}).
