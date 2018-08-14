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
-type contract_id() :: dmsl_domain_thrift:'ContractID'().
-type wallet_id()   :: dmsl_domain_thrift:'WalletID'().

-type party_params() :: #{
    email := binary()
}.

-export_type([id/0]).
-export_type([contract_id/0]).
-export_type([wallet_id/0]).
-export_type([party_params/0]).

-type inaccessibility() ::
    {inaccessible, blocked | suspended}.

-export_type([inaccessibility/0]).

-export([create/1]).
-export([create/2]).
-export([is_accessible/1]).
-export([create_contract/2]).
-export([change_contractor_level/3]).
-export([validate_wallet_terms/5]).

%% Internal types

-type terms() :: dmsl_domain_thrift:'TermSet'().
-type wallet_terms() :: dmsl_domain_thrift:'WalletServiceTerms'() | undefined.
-type currency() :: ff_currency:currency().
-type timestamp() :: ff_time:timestamp_ms().

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
    {error, inaccessibility()}.

is_accessible(ID) ->
    case do_get_party(ID) of
        #domain_Party{blocking = {blocked, _}} ->
            {error, {inaccessible, blocked}};
        #domain_Party{suspension = {suspended, _}} ->
            {error, {inaccessible, suspended}};
        #domain_Party{} ->
            {ok, accessible}
    end.

%%

-type contract_prototype() :: #{
    payinst           := _PaymentInstitutionRef,
    contract_template := dmsl_domain_thrift:'ContractTemplateRef'(),
    contractor_level  := dmsl_domain_thrift:'ContractorIdentificationLevel'()
}.

-spec create_contract(id(), contract_prototype()) ->
    {ok, contract_id()} |
    {error, inaccessibility()} |
    {error, invalid}.

create_contract(ID, Prototype) ->
    do(fun () ->
        ContractID = generate_contract_id(),
        Changeset  = construct_contract_changeset(ContractID, Prototype),
        Claim      = unwrap(do_create_claim(ID, Changeset)),
        accepted   = do_accept_claim(ID, Claim),
        ContractID
    end).

%%

-spec change_contractor_level(id(), contract_id(), dmsl_domain_thrift:'ContractorIdentificationLevel'()) ->
    ok |
    {error, inaccessibility()} |
    {error, invalid}.

change_contractor_level(ID, ContractID, ContractorLevel) ->
    do(fun () ->
        Changeset  = construct_level_changeset(ContractID, ContractorLevel),
        Claim      = unwrap(do_create_claim(ID, Changeset)),
        accepted   = do_accept_claim(ID, Claim),
        ok
    end).

%%

-spec validate_wallet_terms(id(), contract_id(), wallet_id(), currency(), timestamp()) -> Result when
    Result :: {ok, valid} | {error, Error},
    Error :: invalid_terms | party_not_found | party_not_exists_yet | {exception, any()}.

validate_wallet_terms(ID, Contract, WalletID, Currency, Timestamp) ->
    case get_contract_terms(ID, Contract, WalletID, Currency, Timestamp) of
        {ok, #domain_TermSet{wallets = Terms}} ->
            validate_terms_is_reduced(Terms);
        {error, _Reason} = Error ->
            Error
    end.


%% Internal functions

generate_contract_id() ->
    generate_uuid().

generate_uuid() ->
    % TODO
    %  - Snowflake, anyone?
    uuid:uuid_to_string(uuid:get_v4(), binary_nodash).

%% Party management client

do_create_party(ID, Params) ->
    case call('Create', [construct_userinfo(), ID, construct_party_params(Params)]) of
        {ok, ok} ->
            ok;
        {exception, #payproc_PartyExists{}} ->
            {error, exists};
        {exception, Unexpected} ->
            error(Unexpected)
    end.

do_get_party(ID) ->
    case call('Get', [construct_userinfo(), ID]) of
        {ok, #domain_Party{} = Party} ->
            Party;
        {exception, Unexpected} ->
            error(Unexpected)
    end.

% do_get_contract(ID, ContractID) ->
%     case call('GetContract', [construct_userinfo(), ID, ContractID]) of
%         {ok, #domain_Contract{} = Contract} ->
%             Contract;
%         {exception, #payproc_ContractNotFound{}} ->
%             {error, notfound};
%         {exception, Unexpected} ->
%             error(Unexpected)
%     end.

do_create_claim(ID, Changeset) ->
    case call('CreateClaim', [construct_userinfo(), ID, Changeset]) of
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
    case call('AcceptClaim', [construct_userinfo(), ID, ClaimID, Revision]) of
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

construct_userinfo() ->
    #payproc_UserInfo{id = <<"fistful">>, type = construct_usertype()}.

construct_usertype() ->
    {service_user, #payproc_ServiceUser{}}.

%% Woody stuff

call(Function, Args) ->
    % TODO
    %  - Ideally, we should provide `Client` here explicitly.
    Service = {dmsl_payment_processing_thrift, 'PartyManagement'},
    ff_woody_client:call(partymgmt, {Service, Function, Args}).

%% Terms stuff

-spec get_contract_terms(id(), contract_id(), wallet_id(), currency(), timestamp()) -> Result when
    Result :: {ok, terms()} | {error, Error},
    Error :: party_not_found | party_not_exists_yet | {exception, any()}.

get_contract_terms(ID, ContractID, WalletID, Currency, Timestamp) ->
    CurrencyRef = #domain_CurrencyRef{symbolic_code = ff_currency:symcode(Currency)},
    Args = [construct_userinfo(), ID, ContractID, WalletID, CurrencyRef, ff_time:to_rfc3339(Timestamp)],
    case call('ComputeWalletTerms', Args) of
        {ok, Terms} ->
            {ok, Terms};
        {exception, #payproc_PartyNotFound{}} ->
            {error, party_not_found};
        {exception, #payproc_PartyNotExistsYet{}} ->
            {error, party_not_exists_yet};
        {exception, Unexpected} ->
            {error, {exception, Unexpected}}
    end.

-spec validate_terms_is_reduced(wallet_terms()) ->
    {ok, valid} | {error, invalid_terms}.

validate_terms_is_reduced(undefined) ->
    {error, invalid_terms};
validate_terms_is_reduced(Terms) ->
    #domain_WalletServiceTerms{
        currencies = CurrenciesSelector,
        cash_limit = CashSelector,
        turnover_limit = TurnoverSelector
    } = Terms,
    case lists:all(fun selector_is_reduced/1, [CurrenciesSelector, CashSelector, TurnoverSelector]) of
        true ->
            {ok, valid};
        false ->
            {error, invalid_terms}
    end.

selector_is_reduced({value, _Value}) ->
    true;
selector_is_reduced({decisions, _Decisions}) ->
    false.
