REBAR := $(or $(shell which rebar3), $(error "`rebar3' executable missing"))
SUBMODULES = build-utils
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
BUILD_IMAGE_TAG := 562313697353c29d4b34fb081a8b70e8c2207134

CALL_ANYWHERE := all submodules compile xref lint dialyze release clean distclean

CALL_W_CONTAINER := $(CALL_ANYWHERE) test

all: compile

-include $(UTILS_PATH)/make_lib/utils_container.mk
-include $(UTILS_PATH)/make_lib/utils_image.mk

.PHONY: $(CALL_W_CONTAINER)

$(SUBTARGETS): %/.git: %
	git submodule update --init $<
	touch $@

submodules: $(SUBTARGETS)

compile: submodules
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

distclean:
	$(REBAR) clean -a
	rm -rf _build

test: submodules
	$(REBAR) ct

test.%: apps/fistful/test/ff_%_SUITE.erl submodules
	$(REBAR) ct --suite=$<
