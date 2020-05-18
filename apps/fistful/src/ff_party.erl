%%%
%%% Managed party
%%%
%%% TODOs
%%%
%%%  - We expect party to exist, which is certainly not the general case.
%%%


-module(ff_party).

-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").

-type id()          :: dmsl_domain_thrift:'PartyID'().
-type contract_id() :: dmsl_domain_thrift:'ContractID'().
-type wallet_id()   :: dmsl_domain_thrift:'WalletID'().
-type clock()       :: ff_transaction:clock().
-type revision()    :: dmsl_domain_thrift:'PartyRevision'().
-type terms()       :: dmsl_domain_thrift:'TermSet'().

-type party_params() :: #{
    email := binary()
}.

-type validate_account_creation_error() ::
    currency_validation_error().

-type validate_deposit_creation_error() ::
    currency_validation_error() |
    {bad_deposit_amount, Cash :: cash()}.

-type get_contract_terms_error() ::
    {party_not_found, id()} |
    {contract_not_found, id()} |
    {party_not_exists_yet, id()}.

-type validate_withdrawal_creation_error() ::
    currency_validation_error() |
    cash_range_validation_error().

-type validate_p2p_error() ::
    currency_validation_error() |
    p2p_forbidden_error() |
    cash_range_validation_error() |
    invalid_p2p_terms_error().

-type validate_w2w_transfer_creation_error() ::
    w2w_forbidden_error() |
    currency_validation_error() |
    {bad_w2w_transfer_amount, Cash :: cash()}|
    invalid_w2w_terms_error().

-type validate_p2p_template_creation_error() ::
    {bad_p2p_template_amount, Cash :: cash()} |
    p2p_template_forbidden_error().

-export_type([id/0]).
-export_type([revision/0]).
-export_type([terms/0]).
-export_type([contract_id/0]).
-export_type([wallet_id/0]).
-export_type([party_params/0]).
-export_type([validate_deposit_creation_error/0]).
-export_type([validate_account_creation_error/0]).
-export_type([validate_p2p_error/0]).
-export_type([get_contract_terms_error/0]).
-export_type([validate_withdrawal_creation_error/0]).
-export_type([validate_w2w_transfer_creation_error/0]).
-export_type([validate_p2p_template_creation_error/0]).
-export_type([cash/0]).
-export_type([cash_range/0]).

-type inaccessibility() ::
    {inaccessible, blocked | suspended}.

-export_type([inaccessibility/0]).

-export([create/1]).
-export([create/2]).
-export([is_accessible/1]).
-export([create_contract/2]).
-export([get_revision/1]).
-export([change_contractor_level/3]).
-export([validate_account_creation/2]).
-export([validate_withdrawal_creation/2]).
-export([validate_deposit_creation/2]).
-export([validate_w2w_transfer_creation/2]).
-export([validate_p2p_template_creation/1]).
-export([validate_p2p_template_creation/2]).
-export([validate_wallet_limits/3]).
-export([get_contract_terms/6]).
-export([get_withdrawal_cash_flow_plan/1]).
-export([get_p2p_cash_flow_plan/1]).
-export([get_w2w_cash_flow_plan/1]).
-export([validate_p2p/2]).
-export([get_identity_payment_institution_id/1]).

%% Internal types
-type cash() :: ff_cash:cash().
-type wallet_terms() :: dmsl_domain_thrift:'WalletServiceTerms'().
-type withdrawal_terms() :: dmsl_domain_thrift:'WithdrawalServiceTerms'().
-type p2p_terms() :: dmsl_domain_thrift:'P2PServiceTerms'().
-type w2w_terms() :: dmsl_domain_thrift:'W2WServiceTerms'().
-type currency_id() :: ff_currency:id().
-type currency_ref() :: dmsl_domain_thrift:'CurrencyRef'().
-type domain_cash() :: dmsl_domain_thrift:'Cash'().
-type domain_cash_range() :: dmsl_domain_thrift:'CashRange'().
-type domain_revision() :: ff_domain_config:revision().
-type timestamp() :: ff_time:timestamp_ms().
-type wallet() :: ff_wallet:wallet().
-type payment_institution_id() :: ff_payment_institution:id().
-type bound_type() :: 'exclusive' | 'inclusive'.
-type cash_range() :: {{bound_type(), cash()}, {bound_type(), cash()}}.

-type currency_validation_error() :: {terms_violation, {not_allowed_currency,
    {currency_ref(), ordsets:ordset(currency_ref())}
}}.
-type cash_range_validation_error() :: {terms_violation, {cash_range, {cash(), cash_range()}}}.
-type p2p_forbidden_error() :: {terms_violation, p2p_forbidden}.
-type w2w_forbidden_error() :: {terms_violation, w2w_forbidden}.
-type p2p_template_forbidden_error() :: {terms_violation, p2p_template_forbidden}.

-type not_reduced_error() :: {not_reduced, {Name :: atom(), TermsPart :: any()}}.

-type invalid_withdrawal_terms_error() ::
    invalid_wallet_terms_error() |
    {invalid_terms, not_reduced_error()} |
    {invalid_terms, {undefined_withdrawal_terms, wallet_terms()}}.

-type invalid_wallet_terms_error() ::
    {invalid_terms, not_reduced_error()} |
    {invalid_terms, undefined_wallet_terms}.

-type invalid_p2p_terms_error() ::
    {invalid_terms, not_reduced_error()} |
    {invalid_terms, undefined_wallet_terms} |
    {invalid_terms, {undefined_p2p_terms, wallet_terms()}}.

-type invalid_w2w_terms_error() ::
    {invalid_terms, not_reduced_error()} |
    {invalid_terms, undefined_wallet_terms} |
    {invalid_terms, {undefined_w2w_terms, wallet_terms()}}.

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

-spec get_revision(id()) ->
    {ok, revision()} | {error, {party_not_found, id()}}.

get_revision(ID) ->
    {Client, Context} = get_party_client(),
    case party_client_thrift:get_revision(ID, Client, Context) of
        {ok, Revision} ->
            {ok, Revision};
        {error, #payproc_PartyNotFound{}} ->
            {error, {party_not_found, ID}};
        {error, Unexpected} ->
            error(Unexpected)
    end.

%%

-type contract_prototype() :: #{
    payinst           := dmsl_domain_thrift:'PaymentInstitutionRef'(),
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

-spec get_identity_payment_institution_id(ff_identity:identity()) -> Result when
    Result :: {ok, payment_institution_id()} | {error, Error},
    Error ::
        {party_not_found, id()} |
        {contract_not_found, id()} |
        no_return().

get_identity_payment_institution_id(Identity) ->
    do(fun() ->
        PartyID = ff_identity:party(Identity),
        ContractID = ff_identity:contract(Identity),
        Contract = unwrap(do_get_contract(PartyID, ContractID)),
        #domain_PaymentInstitutionRef{id = ID} = Contract#domain_Contract.payment_institution,
        ID
    end).

-spec get_contract_terms(PartyID, ContractID, Varset, Timestamp, PartyRevision, DomainRevision) -> Result when
    PartyID :: id(),
    ContractID :: contract_id(),
    Varset :: hg_selector:varset(),
    Timestamp :: timestamp(),
    PartyRevision :: revision(),
    DomainRevision :: domain_revision(),
    Result :: {ok, terms()} | {error, Error},
    Error :: get_contract_terms_error().

get_contract_terms(PartyID, ContractID, Varset, Timestamp, PartyRevision, DomainRevision) ->
    DomainVarset = encode_varset(Varset),
    TimestampStr = ff_time:to_rfc3339(Timestamp),
    {Client, Context} = get_party_client(),
    Result = party_client_thrift:compute_contract_terms(
        PartyID,
        ContractID,
        TimestampStr,
        {revision, PartyRevision},
        DomainRevision,
        DomainVarset,
        Client,
        Context
    ),
    case Result of
        {ok, Terms} ->
            {ok, Terms};
        {error, #payproc_PartyNotFound{}} ->
            {error, {party_not_found, PartyID}};
        {error, #payproc_ContractNotFound{}} ->
            {error, {contract_not_found, PartyID}};
        {error, #payproc_PartyNotExistsYet{}} ->
            {error, {party_not_exists_yet, PartyID}};
        {error, Unexpected} ->
            erlang:error({unexpected, Unexpected})
    end.

-spec validate_account_creation(terms(), currency_id()) -> Result when
    Result :: {ok, valid} | {error, Error},
    Error :: currency_validation_error().

validate_account_creation(Terms, CurrencyID) ->
    #domain_TermSet{wallets = WalletTerms} = Terms,
    do(fun () ->
        {ok, valid} = validate_wallet_currencies_term_is_reduced(WalletTerms),
        valid = unwrap(validate_wallet_terms_currency(CurrencyID, WalletTerms))
    end).

-spec validate_withdrawal_creation(terms(), cash()) -> Result when
    Result :: {ok, valid} | {error, Error},
    Error :: validate_withdrawal_creation_error().

validate_withdrawal_creation(Terms, {_, CurrencyID} = Cash) ->
    #domain_TermSet{wallets = WalletTerms} = Terms,
    do(fun () ->
        {ok, valid} = validate_withdrawal_terms_is_reduced(WalletTerms),
        valid = unwrap(validate_wallet_terms_currency(CurrencyID, WalletTerms)),
        #domain_WalletServiceTerms{withdrawals = WithdrawalTerms} = WalletTerms,
        valid = unwrap(validate_withdrawal_terms_currency(CurrencyID, WithdrawalTerms)),
        valid = unwrap(validate_withdrawal_cash_limit(Cash, WithdrawalTerms))
    end).

-spec validate_deposit_creation(terms(), cash()) -> Result when
    Result :: {ok, valid} | {error, Error},
    Error :: validate_deposit_creation_error().

validate_deposit_creation(_Terms, {Amount, _Currency} = Cash) when Amount < 1 ->
    {error, {bad_deposit_amount, Cash}};
validate_deposit_creation(Terms, {_Amount, CurrencyID} = _Cash) ->
    do(fun () ->
        #domain_TermSet{wallets = WalletTerms} = Terms,
        {ok, valid} = validate_wallet_currencies_term_is_reduced(WalletTerms),
        valid = unwrap(validate_wallet_terms_currency(CurrencyID, WalletTerms))
    end).

-spec validate_p2p(terms(), cash()) -> Result when
    Result :: {ok, valid} | {error, Error},
    Error :: validate_p2p_error().

validate_p2p(Terms, {_, CurrencyID} = Cash) ->
    #domain_TermSet{wallets = WalletTerms} = Terms,
    do(fun () ->
        {ok, valid} = validate_p2p_terms_is_reduced(WalletTerms),
        #domain_WalletServiceTerms{p2p = P2PServiceTerms} = WalletTerms,
        valid = unwrap(validate_p2p_terms_currency(CurrencyID, P2PServiceTerms)),
        valid = unwrap(validate_p2p_cash_limit(Cash, P2PServiceTerms)),
        valid = unwrap(validate_p2p_allow(P2PServiceTerms))
    end).

-spec validate_w2w_transfer_creation(terms(), cash()) -> Result when
    Result :: {ok, valid} | {error, Error},
    Error :: validate_w2w_transfer_creation_error().

validate_w2w_transfer_creation(_Terms, {Amount, _Currency} = Cash) when Amount < 1 ->
    {error, {bad_w2w_transfer_amount, Cash}};
validate_w2w_transfer_creation(Terms, {_Amount, CurrencyID} = Cash) ->
    #domain_TermSet{wallets = WalletTerms} = Terms,
    do(fun () ->
        {ok, valid} = validate_w2w_terms_is_reduced(WalletTerms),
        #domain_WalletServiceTerms{w2w = W2WServiceTerms} = WalletTerms,
        valid = unwrap(validate_w2w_terms_currency(CurrencyID, W2WServiceTerms)),
        valid = unwrap(validate_w2w_cash_limit(Cash, W2WServiceTerms)),
        valid = unwrap(validate_w2w_allow(W2WServiceTerms))
    end).

-spec validate_p2p_template_creation(terms(), cash()) -> Result when
    Result :: {ok, valid} | {error, Error},
    Error :: validate_p2p_template_creation_error().

validate_p2p_template_creation(Terms, {undefined, _Currency}) ->
    validate_p2p_template_creation(Terms);
validate_p2p_template_creation(_Terms, {Amount, _Currency} = Cash) when Amount < 1 ->
    {error, {bad_p2p_template_amount, Cash}};
validate_p2p_template_creation(Terms, {_Amount, _CurrencyID} = _Cash) ->
    #domain_TermSet{wallets = WalletTerms} = Terms,
    do(fun () ->
        #domain_WalletServiceTerms{p2p = P2PServiceTerms} = WalletTerms,
        valid = unwrap(validate_p2p_template_allow(P2PServiceTerms))
    end).

-spec validate_p2p_template_creation(terms()) -> Result when
    Result :: {ok, valid} | {error, Error},
    Error :: validate_p2p_template_creation_error().

validate_p2p_template_creation(Terms) ->
    #domain_TermSet{wallets = WalletTerms} = Terms,
    do(fun () ->
        #domain_WalletServiceTerms{p2p = P2PServiceTerms} = WalletTerms,
        valid = unwrap(validate_p2p_template_allow(P2PServiceTerms))
    end).

-spec get_withdrawal_cash_flow_plan(terms()) ->
    {ok, ff_cash_flow:cash_flow_plan()} | {error, _Error}.
get_withdrawal_cash_flow_plan(Terms) ->
    #domain_TermSet{
        wallets = #domain_WalletServiceTerms{
            withdrawals = #domain_WithdrawalServiceTerms{
                cash_flow = CashFlow
            }
        }
    } = Terms,
    {value, DomainPostings} = CashFlow,
    Postings = ff_cash_flow:decode_domain_postings(DomainPostings),
    {ok, #{postings => Postings}}.

-spec get_p2p_cash_flow_plan(terms()) ->
    {ok, ff_cash_flow:cash_flow_plan()} | {error, _Error}.
get_p2p_cash_flow_plan(Terms) ->
    #domain_TermSet{
        wallets = #domain_WalletServiceTerms{
            p2p = #domain_P2PServiceTerms{
                cash_flow = CashFlow
            }
        }
    } = Terms,
    {value, DomainPostings} = CashFlow,
    Postings = ff_cash_flow:decode_domain_postings(DomainPostings),
    {ok, #{postings => Postings}}.

-spec get_w2w_cash_flow_plan(terms()) ->
    {ok, ff_cash_flow:cash_flow_plan()} | {error, _Error}.
get_w2w_cash_flow_plan(Terms) ->
    #domain_TermSet{
        wallets = #domain_WalletServiceTerms{
            w2w = #domain_W2WServiceTerms{
                cash_flow = CashFlow
            }
        }
    } = Terms,
    {value, DomainPostings} = CashFlow,
    Postings = ff_cash_flow:decode_domain_postings(DomainPostings),
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
    {Client, Context} = get_party_client(),
    case party_client_thrift:create(ID, construct_party_params(Params), Client, Context) of
        ok ->
            ok;
        {error, #payproc_PartyExists{}} ->
            {error, exists};
        {error, Unexpected} ->
            error(Unexpected)
    end.

do_get_party(ID) ->
    {Client, Context} = get_party_client(),
    Result = do(fun() ->
        Revision = unwrap(party_client_thrift:get_revision(ID, Client, Context)),
        unwrap(party_client_thrift:checkout(ID, {revision, Revision}, Client, Context))
    end),
    case Result of
        {ok, Party} ->
            Party;
        {error, Reason} ->
            error(Reason)
    end.

do_get_contract(ID, ContractID) ->
    {Client, Context} = get_party_client(),
    case party_client_thrift:get_contract(ID, ContractID, Client, Context) of
        {ok, #domain_Contract{} = Contract} ->
            {ok, Contract};
        {error, #payproc_PartyNotFound{}} ->
            {error, {party_not_found, ID}};
        {error, #payproc_ContractNotFound{}} ->
            {error, {contract_not_found, ContractID}};
        {error, Unexpected} ->
            error(Unexpected)
    end.

do_create_claim(ID, Changeset) ->
    {Client, Context} = get_party_client(),
    case party_client_thrift:create_claim(ID, Changeset, Client, Context) of
        {ok, Claim} ->
            {ok, Claim};
        {error, #payproc_InvalidChangeset{
            reason = {invalid_wallet, #payproc_InvalidWallet{reason = {contract_terms_violated, _}}}
        }} ->
            {error, invalid};
        {error, #payproc_InvalidPartyStatus{status = Status}} ->
            {error, construct_inaccessibilty(Status)};
        {error, Unexpected} ->
            error(Unexpected)
    end.

do_accept_claim(ID, Claim) ->
    % TODO
    %  - We assume here that there's only one actor (identity machine) acting in
    %    such a way which may cause conflicts.
    ClaimID  = Claim#payproc_Claim.id,
    Revision = Claim#payproc_Claim.revision,
    {Client, Context} = get_party_client(),
    case party_client_thrift:accept_claim(ID, ClaimID, Revision, Client, Context) of
        ok ->
            accepted;
        {error, #payproc_InvalidClaimStatus{status = {accepted, _}}} ->
            accepted;
        {error, Unexpected} ->
            error(Unexpected)
    end.

get_party_client() ->
    % TODO
    %  - Move auth logic from hellgate to capi the same way as it works
    %    in wapi & fistful. Then the following dirty user_identity hack
    %    will not be necessary anymore.
    Context0 = ff_context:load(),
    WoodyContextWithoutMeta = maps:without([meta], ff_context:get_woody_context(Context0)),
    Context1 = ff_context:set_woody_context(WoodyContextWithoutMeta, Context0),
    Context2 = ff_context:set_user_identity(construct_user_identity(), Context1),
    Client = ff_context:get_party_client(Context2),
    ClientContext = ff_context:get_party_client_context(Context2),
    {Client, ClientContext}.

-spec construct_user_identity() ->
    woody_user_identity:user_identity().
construct_user_identity() ->
    #{
        id    => <<"fistful">>,
        realm => <<"service">>
    }.

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

%% Terms stuff

-spec validate_wallet_currencies_term_is_reduced(wallet_terms() | undefined) ->
    {ok, valid} | {error, {invalid_terms, _Details}}.

validate_wallet_currencies_term_is_reduced(undefined) ->
    {error, {invalid_terms, undefined_wallet_terms}};
validate_wallet_currencies_term_is_reduced(Terms) ->
    #domain_WalletServiceTerms{
        currencies = CurrenciesSelector
    } = Terms,
    do_validate_terms_is_reduced([
        {wallet_currencies, CurrenciesSelector}
    ]).

-spec validate_withdrawal_terms_is_reduced(wallet_terms() | undefined) ->
    {ok, valid} | {error, invalid_withdrawal_terms_error()}.
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

-spec validate_p2p_terms_is_reduced(wallet_terms() | undefined) ->
    {ok, valid} | {error, invalid_p2p_terms_error()}.
validate_p2p_terms_is_reduced(undefined) ->
    {error, {invalid_terms, undefined_wallet_terms}};
validate_p2p_terms_is_reduced(#domain_WalletServiceTerms{p2p = undefined} = WalletTerms) ->
    {error, {invalid_terms, {undefined_p2p_terms, WalletTerms}}};
validate_p2p_terms_is_reduced(Terms) ->
    #domain_WalletServiceTerms{
        p2p = P2PServiceTerms
    } = Terms,
    #domain_P2PServiceTerms{
        currencies = P2PCurrenciesSelector,
        cash_limit = CashLimitSelector,
        cash_flow = CashFlowSelector,
        fees = FeeSelector,
        quote_lifetime = LifetimeSelector
    } = P2PServiceTerms,
    do_validate_terms_is_reduced([
        {p2p_currencies, P2PCurrenciesSelector},
        {p2p_cash_limit, CashLimitSelector},
        {p2p_cash_flow, CashFlowSelector},
        {p2p_fee, FeeSelector},
        {p2p_quote_lifetime, LifetimeSelector}
    ]).

-spec validate_w2w_terms_is_reduced(wallet_terms() | undefined) ->
    {ok, valid} | {error, invalid_w2w_terms_error()}.
validate_w2w_terms_is_reduced(undefined) ->
    {error, {invalid_terms, undefined_wallet_terms}};
validate_w2w_terms_is_reduced(#domain_WalletServiceTerms{w2w = undefined} = WalletTerms) ->
    {error, {invalid_terms, {undefined_w2w_terms, WalletTerms}}};
validate_w2w_terms_is_reduced(Terms) ->
    #domain_WalletServiceTerms{
        w2w = W2WServiceTerms
    } = Terms,
    #domain_W2WServiceTerms{
        currencies = W2WCurrenciesSelector,
        cash_limit = CashLimitSelector,
        cash_flow = CashFlowSelector,
        fees = FeeSelector
    } = W2WServiceTerms,
    do_validate_terms_is_reduced([
        {w2w_currencies, W2WCurrenciesSelector},
        {w2w_cash_limit, CashLimitSelector},
        {w2w_cash_flow, CashFlowSelector},
        {w2w_fee, FeeSelector}
    ]).

-spec do_validate_terms_is_reduced([{atom(), Selector :: any()}]) ->
    {ok, valid} | {error, {invalid_terms, not_reduced_error()}}.
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

-spec validate_wallet_terms_currency(currency_id(), wallet_terms()) ->
    {ok, valid} | {error, currency_validation_error()}.
validate_wallet_terms_currency(CurrencyID, Terms) ->
    #domain_WalletServiceTerms{
        currencies = {value, Currencies}
    } = Terms,
    validate_currency(CurrencyID, Currencies).

-spec validate_wallet_limits(terms(), wallet(), clock()) ->
    {ok, valid} |
    {error, invalid_wallet_terms_error()} |
    {error, cash_range_validation_error()}.
validate_wallet_limits(Terms, Wallet, Clock) ->
    do(fun () ->
        #domain_TermSet{wallets = WalletTerms} = Terms,
        valid = unwrap(validate_wallet_limits_terms_is_reduced(WalletTerms)),
        #domain_WalletServiceTerms{
            wallet_limit = {value, CashRange}
        } = WalletTerms,
        Account = ff_wallet:account(Wallet),
        valid = unwrap(validate_account_balance(Account, CashRange, Clock))
    end).

-spec validate_wallet_limits_terms_is_reduced(wallet_terms()) ->
    {ok, valid} | {error, {invalid_terms, _Details}}.
validate_wallet_limits_terms_is_reduced(Terms) ->
    #domain_WalletServiceTerms{
        wallet_limit = WalletLimitSelector
    } = Terms,
    do_validate_terms_is_reduced([
        {wallet_limit, WalletLimitSelector}
    ]).

-spec validate_withdrawal_terms_currency(currency_id(), withdrawal_terms()) ->
    {ok, valid} | {error, currency_validation_error()}.
validate_withdrawal_terms_currency(CurrencyID, Terms) ->
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
    validate_cash_range(ff_dmsl_codec:marshal(cash, Cash), CashRange).

-spec validate_p2p_terms_currency(currency_id(), p2p_terms()) ->
    {ok, valid} | {error, currency_validation_error()}.
validate_p2p_terms_currency(CurrencyID, Terms) ->
    #domain_P2PServiceTerms{
        currencies = {value, Currencies}
    } = Terms,
    validate_currency(CurrencyID, Currencies).

-spec validate_p2p_cash_limit(cash(), p2p_terms()) ->
    {ok, valid} | {error, cash_range_validation_error()}.
validate_p2p_cash_limit(Cash, Terms) ->
    #domain_P2PServiceTerms{
        cash_limit = {value, CashRange}
    } = Terms,
    validate_cash_range(ff_dmsl_codec:marshal(cash, Cash), CashRange).

-spec validate_p2p_allow(p2p_terms()) ->
    {ok, valid} | {error, p2p_forbidden_error()}.
validate_p2p_allow(P2PServiceTerms) ->
    #domain_P2PServiceTerms{allow = Constant} = P2PServiceTerms,
    case Constant of
        {constant, true} ->
            {ok, valid};
        {constant, false} ->
            {error, {terms_violation, p2p_forbidden}}
    end.

-spec validate_w2w_terms_currency(currency_id(), w2w_terms()) ->
    {ok, valid} | {error, currency_validation_error()}.
validate_w2w_terms_currency(CurrencyID, Terms) ->
    #domain_W2WServiceTerms{
        currencies = {value, Currencies}
    } = Terms,
    validate_currency(CurrencyID, Currencies).

-spec validate_w2w_cash_limit(cash(), w2w_terms()) ->
    {ok, valid} | {error, cash_range_validation_error()}.
validate_w2w_cash_limit(Cash, Terms) ->
    #domain_W2WServiceTerms{
        cash_limit = {value, CashRange}
    } = Terms,
    validate_cash_range(ff_dmsl_codec:marshal(cash, Cash), CashRange).

-spec validate_w2w_allow(w2w_terms()) ->
    {ok, valid} | {error, w2w_forbidden_error()}.
validate_w2w_allow(W2WServiceTerms) ->
    #domain_W2WServiceTerms{allow = Constant} = W2WServiceTerms,
    case Constant of
        {constant, true} ->
            {ok, valid};
        {constant, false} ->
            {error, {terms_violation, w2w_forbidden}}
    end.

-spec validate_p2p_template_allow(p2p_terms()) ->
    {ok, valid} | {error, p2p_forbidden_error()}.
validate_p2p_template_allow(P2PServiceTerms) ->
    #domain_P2PServiceTerms{templates = P2PTemplateServiceTerms} = P2PServiceTerms,
    #domain_P2PTemplateServiceTerms{allow = Constant} = P2PTemplateServiceTerms,
    case Constant of
        {constant, true} ->
            {ok, valid};
        {constant, false} ->
            {error, {terms_violation, w2w_forbidden}}
    end.

-spec validate_currency(currency_id(), ordsets:ordset(currency_ref())) ->
    {ok, valid} | {error, currency_validation_error()}.
validate_currency(CurrencyID, Currencies) ->
    CurrencyRef = #domain_CurrencyRef{symbolic_code = CurrencyID},
    case ordsets:is_element(CurrencyRef, Currencies) of
        true ->
            {ok, valid};
        false ->
            {error, {terms_violation, {not_allowed_currency, {CurrencyRef, Currencies}}}}
    end.

-spec validate_account_balance(ff_account:account(), domain_cash_range(), clock()) ->
    {ok, valid} |
    {error, cash_range_validation_error()}.
validate_account_balance(Account, CashRange, Clock) ->
    do(fun() ->
        {Amounts, CurrencyID} = unwrap(ff_transaction:balance(
                Account,
                Clock
            )),
        ExpMinCash = ff_dmsl_codec:marshal(cash, {ff_indef:expmin(Amounts), CurrencyID}),
        ExpMaxCash = ff_dmsl_codec:marshal(cash, {ff_indef:expmax(Amounts), CurrencyID}),
        valid = unwrap(validate_cash_range(ExpMinCash, CashRange)),
        valid = unwrap(validate_cash_range(ExpMaxCash, CashRange))
    end).

-spec validate_cash_range(domain_cash(), domain_cash_range()) ->
    {ok, valid} | {error, cash_range_validation_error()}.
validate_cash_range(Cash, CashRange) ->
    case is_inside(Cash, CashRange) of
        true ->
            {ok, valid};
        _ ->
            DecodedCash = ff_dmsl_codec:unmarshal(cash, Cash),
            DecodedCashRange = ff_dmsl_codec:unmarshal(cash_range, CashRange),
            {error, {terms_violation, {cash_range, {DecodedCash, DecodedCashRange}}}}
    end.

is_inside(Cash, #domain_CashRange{lower = Lower, upper = Upper}) ->
    compare_cash(fun erlang:'>'/2, Cash, Lower) andalso
        compare_cash(fun erlang:'<'/2, Cash, Upper).

compare_cash(_Fun, V, {inclusive, V}) ->
    true;
compare_cash(
    Fun,
    #domain_Cash{amount = A, currency = C},
    {_, #domain_Cash{amount = Am, currency = C}}
) ->
    Fun(A, Am).

%% Varset stuff

-spec encode_varset(hg_selector:varset()) ->
    dmsl_payment_processing_thrift:'Varset'().

encode_varset(Varset) ->
    #payproc_Varset{
        currency = genlib_map:get(currency, Varset),
        amount = genlib_map:get(cost, Varset),
        wallet_id = genlib_map:get(wallet_id, Varset),
        payment_method = encode_payment_method(genlib_map:get(payment_tool, Varset)),
        p2p_tool = genlib_map:get(p2p_tool, Varset)
    }.

-spec encode_payment_method(ff_destination:resource() | undefined) ->
    dmsl_domain_thrift:'PaymentMethodRef'() | undefined.

encode_payment_method(undefined) ->
    undefined;
encode_payment_method({bank_card, #domain_BankCard{payment_system = PaymentSystem}}) ->
    #domain_PaymentMethodRef{
        id = {bank_card, PaymentSystem}
    };
encode_payment_method({crypto_currency, CryptoCurrency}) ->
    #domain_PaymentMethodRef{
        id = {crypto_currency, CryptoCurrency}
    }.
