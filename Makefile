all: deps compile

deps: deps/yaws

deps/yaws:
	@rebar get-deps

compile:
	@rebar compile

dialyze: all dial_dkb dial_dky

dial_dkb:
	dialyzer -Wrace_conditions dk_bench/ebin

dial_dky:
	dialyzer -Wrace_conditions dk_yaws/ebin

gc:
	@echo 'Removing all emacs backup files'
	@rm -f *~
	@rm -f */*~
	@for CAT in *; do \
    if [[ -d $${CAT} ]]; then \
      if [[ -d $${CAT} && -f $${CAT}/Makefile ]]; then (cd $${CAT}; make gc); fi; \
      for FEAT in $${CAT}/*; do \
        if [[ -d $${FEAT} && -f $${FEAT}/Makefile ]]; then (cd $${FEAT}; make gc); fi \
      done \
    fi \
  done

rel: all
	@echo 'Generating dk_bench release'
	@(cd rel; rebar generate)

clean:
	@rebar clean

relclean: relclean_dkb

relclean_dkb:
	@rm -rf rel/dk_bench

realclean: clean relclean
	@rebar del-deps
	@rm -rf deps/*


eunit: all
	ERL_LIBS=$(CURDIR):$(CURDIR)/deps rebar skip_deps=true eunit
