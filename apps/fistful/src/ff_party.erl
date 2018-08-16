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

-type party_params() :: #{
    email := binary()
}.

-export_type([id/0]).
-export_type([contract/0]).
-export_type([wallet/0]).
-export_type([party_params/0]).

-type inaccessiblity() ::
    {inaccessible, blocked | suspended}.

-export_type([inaccessiblity/0]).

-export([create/1]).
-export([create/2]).
-export([is_accessible/1]).
-export([get_wallet/2]).
-export([get_wallet_account/2]).
-export([is_wallet_accessible/2]).
-export([create_contract/2]).
-export([change_contractor_level/3]).
-export([create_wallet/3]).

%% Pipeline

-import(ff_pipeline, [do/1, unwrap/1]).

%%

-spec create(id()) ->
    ok |
    {error, exists}.

create(ID) ->
    create(ID, #{email => <<"bob@example.org">>}).

-spec create(id(), party_params()) ->
    ok |
    {error, exists}.
create(ID, Params) ->
    do_create_party(ID, Params).

-spec is_accessible(id()) ->
    {ok, accessible} |
    {error, inaccessiblity()}.

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
    contract_template := dmsl_domain_thrift:'ContractTemplateRef'(),
    contractor_level  := dmsl_domain_thrift:'ContractorIdentificationLevel'()
}.

-spec create_contract(id(), contract_prototype()) ->
    {ok, contract()} |
    {error, inaccessiblity()} |
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
    generate_uuid().

%%

-spec change_contractor_level(id(), contract(), dmsl_domain_thrift:'ContractorIdentificationLevel'()) ->
    ok |
    {error, inaccessiblity()} |
    {error, invalid}.

change_contractor_level(ID, ContractID, ContractorLevel) ->
    do(fun () ->
        Changeset  = construct_level_changeset(ContractID, ContractorLevel),
        Claim      = unwrap(do_create_claim(ID, Changeset)),
        accepted   = do_accept_claim(ID, Claim),
        ok
    end).

%%

-type wallet_prototype() :: #{
    name     := binary(),
    currency := ff_currency:id()
}.

-spec create_wallet(id(), contract(), wallet_prototype()) ->
    {ok, wallet()} |
    {error, inaccessiblity()} |
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
    generate_uuid().

generate_uuid() ->
    % TODO
    %  - Snowflake, anyone?
    uuid:uuid_to_string(uuid:get_v4(), binary_nodash).

%% Party management client

do_create_party(ID, Params) ->
    case call('Create', [ID, construct_party_params(Params)]) of
        {ok, ok} ->
            ok;
        {exception, #payproc_PartyExists{}} ->
            {error, exists};
        {exception, Unexpected} ->
            error(Unexpected)
    end.

do_get_party(ID) ->
    case call('Get', [ID]) of
        {ok, #domain_Party{} = Party} ->
            Party;
        {exception, Unexpected} ->
            error(Unexpected)
    end.

% do_get_contract(ID, ContractID) ->
%     case call('GetContract', [ID, ContractID]) of
%         {ok, #domain_Contract{} = Contract} ->
%             Contract;
%         {exception, #payproc_ContractNotFound{}} ->
%             {error, notfound};
%         {exception, Unexpected} ->
%             error(Unexpected)
%     end.

do_get_wallet(ID, WalletID) ->
    case call('GetWallet', [ID, WalletID]) of
        {ok, #domain_Wallet{} = Wallet} ->
            {ok, Wallet};
        {exception, #payproc_WalletNotFound{}} ->
            {error, notfound};
        {exception, Unexpected} ->
            error(Unexpected)
    end.

do_create_claim(ID, Changeset) ->
    case call('CreateClaim', [ID, Changeset]) of
        {ok, Claim} ->
            {ok, Claim};
        {exception, #payproc_InvalidChangeset{
            reason = {invalid_wallet, #payproc_InvalidWallet{reason = {contract_terms_violated, _}}}
        }} ->
            {error, invalid};
        {exception, #payproc_InvalidPartyStatus{status = Status}} ->
            {error, construct_inaccessibilty(Status)};
        {exception, Unexpected} ->
            error(Unexpected)
    end.

do_accept_claim(ID, Claim) ->
    % TODO
    %  - We assume here that there's only one actor (identity machine) acting in
    %    such a way which may cause conflicts.
    ClaimID  = Claim#payproc_Claim.id,
    Revision = Claim#payproc_Claim.revision,
    case call('AcceptClaim', [ID, ClaimID, Revision]) of
        {ok, ok} ->
            accepted;
        {exception, #payproc_InvalidClaimStatus{status = {accepted, _}}} ->
            accepted;
        {exception, Unexpected} ->
            error(Unexpected)
    end.

construct_inaccessibilty({blocking, _}) ->
    {inaccessible, blocked};
construct_inaccessibilty({suspension, _}) ->
    {inaccessible, suspended}.

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

construct_party_params(#{email := Email}) ->
    #payproc_PartyParams{
        contact_info = #domain_PartyContactInfo{
            email = Email
        }
    }.

construct_contract_changeset(ContractID, #{
    payinst           := PayInstRef,
    contract_template := ContractTemplateRef,
    contractor_level  := ContractorLevel
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
        ?contractor_mod(
            ContractID,
            {identification_level_modification, ContractorLevel}
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

construct_level_changeset(ContractID, ContractorLevel) ->
    [
        ?contractor_mod(
            ContractID,
            {identification_level_modification, ContractorLevel}
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
    #payproc_UserInfo{id = <<"fistful">>, type = construct_usertype()}.

construct_usertype() ->
    {service_user, #payproc_ServiceUser{}}.

construct_useridentity() ->
    #{
        id    => <<"fistful">>,
        realm => <<"service">>
    }.

%% Woody stuff

get_woody_ctx() ->
    % TODO
    %  - Move auth logic from hellgate to capi the same way as it works
    %    in wapi & fistful. Then the following dirty user_identity hack
    %    will not be necessary anymore.
    reset_useridentity(ff_woody_ctx:get()).

reset_useridentity(Ctx) ->
    woody_user_identity:put(construct_useridentity(), maps:without([meta], Ctx)).

call(Function, Args0) ->
    % TODO
    %  - Ideally, we should provide `Client` here explicitly.
    Service  = {dmsl_payment_processing_thrift, 'PartyManagement'},
    Args     = [construct_userinfo() | Args0],
    ff_woody_client:call(partymgmt, {Service, Function, Args}, get_woody_ctx()).
