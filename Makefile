PROJECT = auths
PROJECT_DESCRIPTION = Authorize
PROJECT_VERSION = 0.1.0

DEPS = cowboy jsone epgsql
dep_cowboy_commit = 2.8.0
dep_jsone_commit = v0.3.3
dep_epgsql_commit = 4.7.1

DEP_PLUGINS = cowboy

BUILD_DEPS += relx
include erlang.mk
