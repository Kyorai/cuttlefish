PROJECT = cuttlefish
PROJECT_DESCRIPTION = cuttlefish configuration abstraction
PROJECT_VERSION = v3.6.0

dep_getopt = hex 1.0.3
DEPS = getopt

PLT_APPS += getopt
EUNIT_OPTS = no_tty, {report, {eunit_progress, [colored, profile]}}

include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME),erlang.mk)
