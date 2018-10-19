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

-type term_varset() :: #{
    amount => cash(),
    wallet_id => wallet_id(),
    currency_id => currency_id()
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
-export([validate_account_creation/2]).
-export([validate_withdrawal_creation/3]).

%% TODO pfffff can't check it on creation :(
-export([validate_wallet_limit/2]).

-export([get_contract_terms/4]).
-export([get_withdrawal_cash_flow_plan/1]).

%% Internal types

-type cash() :: ff_transaction:body().
-type terms() :: dmsl_domain_thrift:'TermSet'().
-type wallet_terms() :: dmsl_domain_thrift:'WalletServiceTerms'() | undefined.
-type withdrawal_terms() :: dmsl_domain_thrift:'WithdrawalServiceTerms'().
-type currency_id() :: ff_currency:id().
-type currency_ref() :: dmsl_domain_thrift:'CurrencyRef'().
-type domain_cash() :: dmsl_domain_thrift:'Cash'().
-type cash_range() :: dmsl_domain_thrift:'CashRange'().
-type timestamp() :: ff_time:timestamp_ms().

-type currency_validation_error() :: {terms_violation, {not_allowed_currency, _Details}}.
-type cash_range_validation_error() :: {terms_violation, {cash_range, {domain_cash(), cash_range()}}}.

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

-spec get_contract_terms(id(), contract_id(), term_varset(), timestamp()) -> Result when
    Result :: {ok, terms()} | {error, Error},
    Error :: {party_not_found, id()} | {party_not_exists_yet, id()} | {exception, any()}.

get_contract_terms(ID, ContractID, Varset, Timestamp) ->
    DomainVarset = encode_varset(Varset),
    Args = [ID, ContractID, ff_time:to_rfc3339(Timestamp), DomainVarset],
    case call('ComputeWalletTermsNew', Args) of
        {ok, Terms} ->
            {ok, Terms};
        {exception, #payproc_PartyNotFound{}} ->
            {error, {party_not_found, ID}};
        {exception, #payproc_PartyNotExistsYet{}} ->
            {error, {party_not_exists_yet, ID}};
        {exception, Unexpected} ->
            {error, {exception, Unexpected}}
    end.

-spec validate_account_creation(terms(), currency_id()) -> Result when
    Result :: {ok, valid} | {error, Error},
    Error ::
        {invalid_terms, _Details} |
        currency_validation_error().

validate_account_creation(Terms, CurrencyID) ->
    #domain_TermSet{wallets = WalletTerms} = Terms,
    do(fun () ->
        valid = unwrap(validate_wallet_creation_terms_is_reduced(WalletTerms)),
        valid = unwrap(validate_wallet_currency(CurrencyID, WalletTerms))
    end).

-spec validate_withdrawal_creation(terms(), cash(), ff_account:account()) -> Result when
    Result :: {ok, valid} | {error, Error},
    Error ::
        {invalid_terms, _Details} |
        currency_validation_error().

validate_withdrawal_creation(Terms, {_, CurrencyID} = Cash, Account) ->
    #domain_TermSet{wallets = WalletTerms} = Terms,
    do(fun () ->
        valid = unwrap(validate_withdrawal_terms_is_reduced(WalletTerms)),
        #domain_WalletServiceTerms{withdrawals = WithdrawalTerms} = WalletTerms,
        valid = unwrap(validate_withdrawal_wallet_currency(CurrencyID, Account)),
        valid = unwrap(validate_withdrawal_currency(CurrencyID, WithdrawalTerms)),
        valid = unwrap(validate_withdrawal_cash_limit(Cash, WithdrawalTerms))
    end).

-spec get_withdrawal_cash_flow_plan(terms()) ->
    {ok, ff_cash_flow:cash_flow_plan()} | {error, _Error}.
get_withdrawal_cash_flow_plan(Terms) ->
    #domain_TermSet{
        wallets = #domain_WalletServiceTerms{
            withdrawals = #domain_WithdrawalServiceTerms{
                cash_flow = {value, DomainPostings}
            }
        }
    } = Terms,
    Postings = decode_domain_postings(DomainPostings),
    {ok, #{postings => Postings}}.

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

-spec validate_wallet_creation_terms_is_reduced(wallet_terms()) ->
    {ok, valid} | {error, {invalid_terms, _Details}}.

validate_wallet_creation_terms_is_reduced(undefined) ->
    {error, {invalid_terms, undefined_wallet_terms}};
validate_wallet_creation_terms_is_reduced(Terms) ->
    #domain_WalletServiceTerms{
        currencies = CurrenciesSelector,
        wallet_limit = WalletLimitSelector
    } = Terms,
    do_validate_terms_is_reduced([
        {wallet_currencies, CurrenciesSelector},
        {wallet_limit, WalletLimitSelector}
    ]).

-spec validate_withdrawal_terms_is_reduced(wallet_terms()) ->
    {ok, valid} | {error, {invalid_terms, _Details}}.
validate_withdrawal_terms_is_reduced(undefined) ->
    {error, {invalid_terms, undefined_wallet_terms}};
validate_withdrawal_terms_is_reduced(#domain_WalletServiceTerms{withdrawals = undefined} = WalletTerms) ->
    {error, {invalid_terms, {undefined_withdrawal_terms, WalletTerms}}};
validate_withdrawal_terms_is_reduced(Terms) ->
    #domain_WalletServiceTerms{
        currencies = WalletCurrenciesSelector,
        withdrawals = WithdrawalTerms
    } = Terms,
    #domain_WithdrawalServiceTerms{
        currencies = WithdrawalCurrenciesSelector,
        cash_limit = CashLimitSelector,
        cash_flow = CashFlowSelector
    } = WithdrawalTerms,
    do_validate_terms_is_reduced([
        {wallet_currencies, WalletCurrenciesSelector},
        {withdrawal_currencies, WithdrawalCurrenciesSelector},
        {withdrawal_cash_limit, CashLimitSelector},
        {withdrawal_cash_flow, CashFlowSelector}
    ]).

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

-spec validate_wallet_currency(currency_id(), wallet_terms()) ->
    {ok, valid} | {error, currency_validation_error()}.
validate_wallet_currency(CurrencyID, Terms) ->
    #domain_WalletServiceTerms{
        currencies = {value, Currencies}
    } = Terms,
    validate_currency(CurrencyID, Currencies).

-spec validate_wallet_limit(ff_account:account(), terms()) ->
    {ok, valid} | {error, cash_range_validation_error()}.
validate_wallet_limit(Account, #domain_TermSet{wallets = WalletTerms}) ->
    do(fun () ->
        valid = unwrap(validate_wallet_creation_terms_is_reduced(WalletTerms)),
        #domain_WalletServiceTerms{
            wallet_limit = {value, CashRange}
        } = WalletTerms,
        {Amounts, CurrencyID} = unwrap(ff_transaction:balance(
            ff_account:accounter_account_id(Account)
        )),
        ExpMinCash = encode_cash({ff_indef:expmin(Amounts), CurrencyID}),
        ExpMaxCash = encode_cash({ff_indef:expmax(Amounts), CurrencyID}),
        valid = unwrap(validate_cash_range(ExpMinCash, CashRange)),
        valid = unwrap(validate_cash_range(ExpMaxCash, CashRange))
    end).

-spec validate_withdrawal_wallet_currency(currency_id(), ff_account:account()) ->
    {ok, valid} | {error, {invalid_withdrawal_currency, currency_id(), {wallet_currency, currency_id()}}}.
validate_withdrawal_wallet_currency(CurrencyID, Account) ->
    case ff_account:currency(Account) of
        CurrencyID ->
            {ok, valid};
        OtherCurrencyID ->
            {error, {invalid_withdrawal_currency, CurrencyID, {wallet_currency, OtherCurrencyID}}}
    end.

-spec validate_withdrawal_currency(currency_id(), withdrawal_terms()) ->
    {ok, valid} | {error, currency_validation_error()}.
validate_withdrawal_currency(CurrencyID, Terms) ->
    #domain_WithdrawalServiceTerms{
        currencies = {value, Currencies}
    } = Terms,
    validate_currency(CurrencyID, Currencies).

-spec validate_withdrawal_cash_limit(cash(), withdrawal_terms()) ->
    {ok, valid} | {error, cash_range_validation_error()}.
validate_withdrawal_cash_limit(Cash, Terms) ->
    #domain_WithdrawalServiceTerms{
        cash_limit = {value, CashRange}
    } = Terms,
    validate_cash_range(encode_cash(Cash), CashRange).

-spec validate_currency(currency_id(), ordsets:ordset(currency_ref())) ->
    {ok, valid} | {error, currency_validation_error()}.
validate_currency(CurrencyID, Currencies) ->
    CurrencyRef = #domain_CurrencyRef{symbolic_code = CurrencyID},
    case ordsets:is_element(CurrencyRef, Currencies) of
        true ->
            {ok, valid};
        false ->
            {error, {terms_violation, {not_allowed_currency, {CurrencyID, Currencies}}}}
    end.

-spec validate_cash_range(domain_cash(), cash_range()) ->
    {ok, valid} | {error, cash_range_validation_error()}.
validate_cash_range(Cash, CashRange) ->
    case is_inside(Cash, CashRange) of
        true ->
            {ok, valid};
        _ ->
            {error, {terms_violation, {cash_range, {Cash, CashRange}}}}
    end.

is_inside(Cash, CashRange = #domain_CashRange{lower = Lower, upper = Upper}) ->
    case {
        compare_cash(fun erlang:'>'/2, Cash, Lower),
        compare_cash(fun erlang:'<'/2, Cash, Upper)
    } of
        {true, true} ->
            true;
        {false, true} ->
            false;
        {true, false} ->
            false;
        {error, currency_missmatch} ->
            {error, currency_missmatch}
    end.

compare_cash(_, V, {inclusive, V}) ->
    true;
compare_cash(
    F,
    #domain_Cash{amount = A, currency = C},
    {_, #domain_Cash{amount = Am, currency = C}}
) ->
    F(A, Am);
compare_cash(_, _, _) ->
    {error, currency_missmatch}.

%% Domain cash flow unmarshalling

-spec decode_domain_postings(ff_cash_flow:domain_plan_postings()) ->
    [ff_cash_flow:plan_posting()].
decode_domain_postings(DomainPostings) ->
    [decode_domain_posting(P) || P <- DomainPostings].

-spec decode_domain_posting(dmsl_domain_thrift:'CashFlowPosting'()) ->
    ff_cash_flow:plan_posting().
decode_domain_posting(
    #domain_CashFlowPosting{
        source = Source,
        destination = Destination,
        volume = Volume,
        details = Details
    }
) ->
    #{
        sender => decode_domain_plan_account(Source),
        receiver => decode_domain_plan_account(Destination),
        volume => decode_domain_plan_volume(Volume),
        details => Details
    }.

-spec decode_domain_plan_account(dmsl_domain_thrift:'CashFlowAccount'()) ->
    ff_cash_flow:plan_account().
decode_domain_plan_account({_AccountNS, _AccountType} = Account) ->
    Account.

-spec decode_domain_plan_volume(dmsl_domain_thrift:'CashVolume'()) ->
    ff_cash_flow:plan_volume().
decode_domain_plan_volume({fixed, #domain_CashVolumeFixed{cash = Cash}}) ->
    {fixed, decode_domain_cash(Cash)};
decode_domain_plan_volume({share, Share}) ->
    #domain_CashVolumeShare{
        parts = Parts,
        'of' = Of,
        rounding_method = RoundingMethod
    } = Share,
    {share, {decode_rational(Parts), Of, decode_rounding_method(RoundingMethod)}};
decode_domain_plan_volume({product, {Fun, CVs}}) ->
    {product, {Fun, lists:map(fun decode_domain_plan_volume/1, CVs)}}.

-spec decode_rounding_method(dmsl_domain_thrift:'RoundingMethod'() | undefined) ->
    ff_cash_flow:rounding_method().
decode_rounding_method(undefined) ->
    default;
decode_rounding_method(RoundingMethod) ->
    RoundingMethod.

-spec decode_rational(dmsl_base_thrift:'Rational'()) ->
    genlib_rational:t().
decode_rational(#'Rational'{p = P, q = Q}) ->
    genlib_rational:new(P, Q).

-spec decode_domain_cash(domain_cash()) ->
    ff_cash_flow:cash().
decode_domain_cash(
    #domain_Cash{
        amount = Amount,
        currency = #domain_CurrencyRef{
            symbolic_code = SymbolicCode
        }
    }
) ->
    {Amount, SymbolicCode}.

%% Varset stuff

-spec encode_varset(term_varset()) ->
    dmsl_payment_processing_thrift:'Varset'().
encode_varset(Varset) ->
    #payproc_Varset{
        currency = encode_currency(genlib_map:get(currency_id, Varset)),
        amount = encode_cash(genlib_map:get(amount, Varset)),
        wallet_id = genlib_map:get(wallet_id, Varset)
    }.

-spec encode_currency(currency_id() | undefined) ->
    currency_ref() | undefined.
encode_currency(undefined) ->
    undefined;
encode_currency(CurrencyID) ->
    #domain_CurrencyRef{symbolic_code = CurrencyID}.

-spec encode_cash(cash() | undefined) ->
    domain_cash() | undefined.
encode_cash(undefined) ->
    undefined;
encode_cash({Amount, CurrencyID}) ->
    #domain_Cash{
        amount = Amount,
        currency = #domain_CurrencyRef{
            symbolic_code = CurrencyID
        }
    }.
