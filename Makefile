REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
SUBMODULES = build-utils

# wapi
SUBMODULES += schemes/swag
SUBTARGETS = $(patsubst %,%/.git,$(SUBMODULES))

UTILS_PATH := build-utils
TEMPLATES_PATH := .

# Name of the service
SERVICE_NAME := fistful-server
# Service image default tag
SERVICE_IMAGE_TAG ?= $(shell git rev-parse HEAD)
# The tag for service image to be pushed with
SERVICE_IMAGE_PUSH_TAG ?= $(SERVICE_IMAGE_TAG)

# Base image for the service
BASE_IMAGE_NAME := service-erlang
BASE_IMAGE_TAG := da0ab769f01b650b389d18fc85e7418e727cbe96

# Build image tag to be used
BUILD_IMAGE_TAG := 442c2c274c1d8e484e5213089906a4271641d95e

REGISTRY := dr2.rbkmoney.com

CALL_ANYWHERE := all submodules compile xref lint format check_format dialyze release clean distclean
CALL_ANYWHERE += generate regenerate

CALL_W_CONTAINER := $(CALL_ANYWHERE) test

all: compile

-include $(UTILS_PATH)/make_lib/utils_container.mk
-include $(UTILS_PATH)/make_lib/utils_image.mk

.PHONY: $(CALL_W_CONTAINER)

$(SUBTARGETS): %/.git: %
	git submodule update --init $<
	touch $@

submodules: $(SUBTARGETS)

compile: submodules generate
	$(REBAR) compile

xref: submodules
	$(REBAR) xref

lint:
	elvis rock

check_format:
	$(REBAR) fmt -c

format:
	$(REBAR) fmt -w

dialyze: submodules generate
	$(REBAR) dialyzer

release: submodules generate
	$(REBAR) as prod release

clean:
	$(REBAR) clean

distclean: swagger.distclean.server.wallet swagger.distclean.client.wallet swagger.distclean.client.payres
	$(REBAR) clean -a
	rm -rf _build

test: submodules
	$(REBAR) eunit
	$(REBAR) ct

TESTSUITES = $(wildcard apps/*/test/*_SUITE.erl)

define testsuite

test.$(patsubst ff_%_SUITE.erl,%,$(notdir $(1))): $(1) submodules
	$(REBAR) ct --suite=$$<

endef

$(foreach suite,$(TESTSUITES),$(eval $(call testsuite,$(suite))))

# Swagger

.PHONY: generate
generate: swagger.generate.server.wallet swagger.generate.client.wallet swagger.generate.client.payres

.PHONY: regenerate
regenerate: swagger.regenerate.server.wallet swagger.regenerate.client.wallet swagger.generate.client.payres

#

SWAGGER_CODEGEN        = $(call which, swagger-codegen)
SWAGGER_SPEC_BASE_PATH = schemes/swag/api

swagger-spec-path = $(SWAGGER_SPEC_BASE_PATH)/$(1)/swagger.yaml
swagger-app-target = apps/swag_$(1)_$(2)

__swagger_role = $(word 1,$(subst ., ,$*))
__swagger_spec = $(word 2,$(subst ., ,$*))

swagger.generate.%:
	@$(MAKE) \
		SWAGGER_APP_ROLE=$(__swagger_role) \
		SWAGGER_APP_SPEC=$(__swagger_spec) \
		$(call swagger-app-target,$(__swagger_role),$(__swagger_spec))

swagger.distclean.%:
	rm -rf $(call swagger-app-target,$(__swagger_role),$(__swagger_spec))

swagger.regenerate.%:
	@$(MAKE) swagger.distclean.$* swagger.generate.$*

apps/swag_$(SWAGGER_APP_ROLE)_%: $(call swagger-spec-path,%)
	$(SWAGGER_CODEGEN) generate \
		-i $< -l erlang-$(SWAGGER_APP_ROLE) -o $@ \
		--additional-properties packageName=swag_$(SWAGGER_APP_ROLE)_$(SWAGGER_APP_SPEC)
	touch $@
