PROJECT = cxy

DEPS = dk_cxy relx
dep_dk_cxy = https://github.com/duomark/dk_cxy.git 0.9.6
dep_relx   = https://github.com/erlware/relx.git v0.5.2

ERL_LIBS = deps

DIALYZER_OPTS ?= -Werror_handling -Wrace_Conditions -Wunmatched_returns \
	-I include -I deps/*/include

include erlang.mk

shell:
	erl -pa ./ebin -pa deps/dk_cxy/ebin -boot start_sasl -s cxy
