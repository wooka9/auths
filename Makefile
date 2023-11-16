PROJECT = auths
PROJECT_DESCRIPTION = Authorize
PROJECT_VERSION = 0.1.0

DEPS = cowboy jsone
dep_cowboy_commit = 2.8.0

DEP_PLUGINS = cowboy

BUILD_DEPS += relx
include erlang.mk
