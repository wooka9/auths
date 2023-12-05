PROJECT = auths
PROJECT_DESCRIPTION = Authorize
PROJECT_VERSION = 0.1.3

DEPS = cowboy jsone epgsql poolboy
dep_cowboy_commit = 2.8.0
dep_jsone_commit = v0.3.3
dep_epgsql_commit = 4.7.1
dep_poolboy_commit = 1.5.2

DEP_PLUGINS = cowboy

BUILD_DEPS += relx
include erlang.mk
