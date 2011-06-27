all:
	@rebar compile

clean: gc
	@rebar clean

gc:
	@rm -f *~
	@rm -f */*~
	@rm -f */*/*~

eunit: all
	ERL_LIBS=$(CURDIR):$(CURDIR)/deps rebar skip_deps=true eunit

dialyze: all
	dialyzer -Wrace_conditions ebin

