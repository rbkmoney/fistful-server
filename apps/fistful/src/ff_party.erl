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
-export([validate_account_creation/5]).

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

-spec validate_account_creation(id(), contract_id(), wallet_id(), currency(), timestamp()) -> Result when
    Result :: {ok, valid} | {error, Error},
    Error ::
        {invalid_terms, _Details} |
        {party_not_found, id()} |
        {party_not_exists_yet, id()} |
        {exception, any()}.

validate_account_creation(ID, Contract, WalletID, Currency, Timestamp) ->
    case get_contract_terms(ID, Contract, WalletID, Currency, Timestamp) of
        {ok, #domain_TermSet{wallets = Terms}} ->
            do(fun () ->
                valid = unwrap(validate_wallet_creation_terms_is_reduced(Terms)),
                valid = unwrap(validate_currency(Terms, Currency))
            end);
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


%% Terms stuff

-spec get_contract_terms(id(), contract_id(), wallet_id(), currency(), timestamp()) -> Result when
    Result :: {ok, terms()} | {error, Error},
    Error :: {party_not_found, id()} | {party_not_exists_yet, id()} | {exception, any()}.

get_contract_terms(ID, ContractID, WalletID, Currency, Timestamp) ->
    CurrencyRef = #domain_CurrencyRef{symbolic_code = ff_currency:symcode(Currency)},
    Args = [ID, ContractID, WalletID, CurrencyRef, ff_time:to_rfc3339(Timestamp)],
    case call('ComputeWalletTerms', Args) of
        {ok, Terms} ->
            {ok, Terms};
        {exception, #payproc_PartyNotFound{}} ->
            {error, {party_not_found, ID}};
        {exception, #payproc_PartyNotExistsYet{}} ->
            {error, {party_not_exists_yet, ID}};
        {exception, Unexpected} ->
            {error, {exception, Unexpected}}
    end.

-spec validate_wallet_creation_terms_is_reduced(wallet_terms()) ->
    {ok, valid} | {error, {invalid_terms, _Details}}.

validate_wallet_creation_terms_is_reduced(undefined) ->
    {error, {invalid_terms, undefined_wallet_terms}};
validate_wallet_creation_terms_is_reduced(Terms) ->
    #domain_WalletServiceTerms{
        currencies = CurrenciesSelector
    } = Terms,
    do_validate_terms_is_reduced([{currencies, CurrenciesSelector}]).

do_validate_terms_is_reduced([]) ->
    {ok, valid};
do_validate_terms_is_reduced([{Name, Terms} | TermsTail]) ->
    case selector_is_reduced(Terms) of
        Result when Result =:= reduced orelse Result =:= is_undefined ->
            do_validate_terms_is_reduced(TermsTail);
        not_reduced ->
            {error, {invalid_terms, {not_reduced, {Name, Terms}}}}
    end.

selector_is_reduced(undefined) ->
    is_undefined;
selector_is_reduced({value, _Value}) ->
    reduced;
selector_is_reduced({decisions, _Decisions}) ->
    not_reduced.

-spec validate_currency(wallet_terms(), currency()) ->
    {ok, valid} | {error, {invalid_terms, {not_allowed_currency, _Details}}}.

validate_currency(Terms, Currency) ->
    #domain_WalletServiceTerms{
        currencies = {value, Currencies}
    } = Terms,
    CurrencyRef = #domain_CurrencyRef{symbolic_code = ff_currency:symcode(Currency)},
    case ordsets:is_element(CurrencyRef, Currencies) of
        true ->
            {ok, valid};
        false ->
            {error, {invalid_terms, {not_allowed_currency, {CurrencyRef, Currencies}}}}
    end.