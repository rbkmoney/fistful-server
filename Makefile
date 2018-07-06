REBAR = $(or $(shell which rebar3), $(error "`rebar3' executable missing"))
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
BASE_IMAGE_NAME := service_erlang
BASE_IMAGE_TAG := 16e2b3ef17e5fdefac8554ced9c2c74e5c6e9e11

# Build image tag to be used
BUILD_IMAGE_TAG := 585ec439a97bade30cfcebc36cefdb45f13f3372

CALL_ANYWHERE := all submodules compile xref lint dialyze release clean distclean
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

dialyze: submodules
	$(REBAR) dialyzer

release: submodules
	$(REBAR) as prod release

clean:
	$(REBAR) clean

distclean: swag_server.distclean swag_client.distclean
	$(REBAR) clean -a
	rm -rf _build

test: submodules
	$(REBAR) eunit ct

TESTSUITES = $(wildcard apps/*/test/*_SUITE.erl)

define testsuite

test.$(patsubst ff_%_SUITE.erl,%,$(notdir $(1))): $(1) submodules
	$(REBAR) ct --suite=$$<

endef

$(foreach suite,$(TESTSUITES),$(eval $(call testsuite,$(suite))))


#
# wapi
#

.PHONY: generate regenerate swag_server.generate swag_server.regenerate swag_client.generate swag_client.regenerate

generate: swag_server.generate swag_client.generate

regenerate: swag_server.regenerate swag_client.regenerate

SWAGGER_CODEGEN = $(call which, swagger-codegen)
SWAGGER_SCHEME_BASE_PATH := schemes/swag
APP_PATH := apps
SWAGGER_SCHEME_API_PATH := $(SWAGGER_SCHEME_BASE_PATH)/api
SWAG_SPEC_FILE := swagger.yaml

# Swagger server

SWAG_SERVER_PREFIX := swag_server
SWAG_SERVER_APP_TARGET := $(APP_PATH)/$(SWAG_SERVER_PREFIX)
SWAG_SERVER_APP_PATH := $(APP_PATH)/$(SWAG_SERVER_PREFIX)

SWAG_SERVER_APP_TARGET_WALLET  := $(SWAG_SERVER_APP_PATH)_wallet/rebar.config
SWAG_SERVER_APP_TARGET_PAYRES  := $(SWAG_SERVER_APP_PATH)_payres/rebar.config
SWAG_SERVER_APP_TARGET_PRIVDOC := $(SWAG_SERVER_APP_PATH)_privdoc/rebar.config

$(SWAG_SERVER_APP_PATH)_%/rebar.config: $(SWAGGER_SCHEME_BASE_PATH)/.git
	$(SWAGGER_CODEGEN) generate \
		-i $(SWAGGER_SCHEME_API_PATH)/$*/$(SWAG_SPEC_FILE) \
		-l erlang-server \
		-o $(SWAG_SERVER_APP_PATH)_$* \
		--additional-properties \
			packageName=$(SWAG_SERVER_PREFIX)_$*

swag_server.generate: $(SWAG_SERVER_APP_TARGET_WALLET) $(SWAG_SERVER_APP_TARGET_PAYRES) $(SWAG_SERVER_APP_TARGET_PRIVDOC)

swag_server.distclean: swag_server.distclean_wallet swag_server.distclean_payres swag_server.distclean_privdoc

swag_server.distclean_%:
	rm -rf $(SWAG_SERVER_APP_PATH)_$*

swag_server.regenerate: swag_server.distclean swag_server.generate

# Swagger client

SWAG_CLIENT_PREFIX := swag_client
SWAG_CLIENT_APP_TARGET := $(APP_PATH)/$(SWAG_CLIENT_PREFIX)
SWAG_CLIENT_APP_PATH := $(APP_PATH)/$(SWAG_CLIENT_PREFIX)

SWAG_CLIENT_APP_TARGET_WALLET  := $(SWAG_CLIENT_APP_PATH)_wallet/rebar.config
SWAG_CLIENT_APP_TARGET_PAYRES  := $(SWAG_CLIENT_APP_PATH)_payres/rebar.config
SWAG_CLIENT_APP_TARGET_PRIVDOC := $(SWAG_CLIENT_APP_PATH)_privdoc/rebar.config

$(SWAG_CLIENT_APP_PATH)_%/rebar.config: $(SWAGGER_SCHEME_BASE_PATH)/.git
	$(SWAGGER_CODEGEN) generate \
		-i $(SWAGGER_SCHEME_API_PATH)/$*/$(SWAG_SPEC_FILE) \
		-l erlang-client \
		-o $(SWAG_CLIENT_APP_PATH)_$* \
		--additional-properties \
			packageName=$(SWAG_CLIENT_PREFIX)_$*

swag_client.generate: $(SWAG_CLIENT_APP_TARGET_WALLET) $(SWAG_CLIENT_APP_TARGET_PAYRES) $(SWAG_CLIENT_APP_TARGET_PRIVDOC)

swag_client.distclean: swag_client.distclean_wallet swag_client.distclean_payres swag_client.distclean_privdoc

swag_client.distclean_%:
	rm -rf $(SWAG_CLIENT_APP_PATH)_$*

swag_client.regenerate: swag_client.distclean swag_client.generate
