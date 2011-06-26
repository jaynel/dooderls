compile:
	@rebar compile

clean: gc
	@rebar clean

gc:
	@rm -f *~
	@rm -f */*~
	@rm -f */*/*~

eunit: compile
        ERL_LIBS=$(CURDIR):$(CURDIR)/deps rebar skip_deps=true eunit

dialyze: compile
        @rebar skip_deps=true dialyze

rel: compile
	@echo 'Generating release'
	@rebar generate

