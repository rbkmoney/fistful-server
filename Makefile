REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
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
BASE_IMAGE_NAME := service-erlang
BASE_IMAGE_TAG := c0aee9a464ee26b8887dd9660dca69d4c3444179

# Build image tag to be used
BUILD_IMAGE_NAME := build-erlang
BUILD_IMAGE_TAG := 9bedaf514a40f758f1e94d3d542e009bf21d96c1

CALL_ANYWHERE := all submodules rebar-update compile xref lint dialyze plt_update \
				release clean distclean format check_format

CALL_W_CONTAINER := $(CALL_ANYWHERE) test

all: compile

-include $(UTILS_PATH)/make_lib/utils_container.mk
-include $(UTILS_PATH)/make_lib/utils_image.mk

.PHONY: $(CALL_W_CONTAINER)

# CALL_ANYWHERE
$(SUBTARGETS): %/.git: %
	git submodule update --init $<
	touch $@

submodules: $(SUBTARGETS)

rebar-update:
	$(REBAR) update

compile: submodules rebar-update
	$(REBAR) compile

xref: submodules
	$(REBAR) xref

lint:
	elvis rock -V

check_format:
	$(REBAR) fmt -c

format:
	$(REBAR) fmt -w

dialyze: submodules
	$(REBAR) dialyzer

plt_update:
	$(REBAR) dialyzer -u true -s false


release: submodules
	$(REBAR) as prod release

clean:
	$(REBAR) clean

distclean:
	$(REBAR) clean -a
	rm -rf _build

# CALL_W_CONTAINER
test: submodules
	$(REBAR) do eunit, ct
