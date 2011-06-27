all: mqs dkb

mqs:
	@(cd mq_speed; make all)

dkb:
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
        @rebar skip_deps=true dialyze

rel: all
	@echo 'Generating release'
	@rebar generate

