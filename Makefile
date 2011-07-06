all: deps/yaws compile

deps/yaws:
	@rebar get-deps

compile:
	@rebar compile

dialyze: dial_dkb

dial_dkb:
	dialyzer -Wrace_conditions dk_bench/ebin

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

clean: clean_dkb

clean_dkb:
	@(cd dk_bench; make clean)

relclean: relclean_dkb

relclean_dkb:
	@rm -rf rel/dk_bench

realclean: clean relclean
	@rebar del-deps
	@rm -rf deps/*

