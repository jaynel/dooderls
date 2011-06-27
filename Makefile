all: dkb

dkb:
	@(cd dk_bench; make all)

dialyze: dial_dkb

dial_dkb:
	@(cd dk_bench; make dialyze)

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

