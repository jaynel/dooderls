PROJECT = tt_redis_api

DEPS = eredis relx
dep_eredis = https://github.com/wooga/eredis v1.0.4
dep_relx   = https://github.com/erlware/relx v0.5.0

ERL_LIBS = deps

DIALYZER_OPTS ?= -Werror_handling -Wrace_Conditions -Wunmatched_returns \
	-I include -I deps/*/include

include erlang.mk
