all:
	@(cd dk_bench; make all)

dialyze: dial_bench dial_mqs

dial_bench:
	dialyzer -Wrace_conditions dk_bench/ebin

dial_mqs:
	dialyzer -Wrace_conditions dk_bench/mq_speed/ebin

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

rel:
	@echo Generating dk_bench release
	(cd dk_bench/rel; rebar generate)

clean:
	@rebar clean

realclean: clean
	@rebar del-deps
	@rm -rf deps/*
