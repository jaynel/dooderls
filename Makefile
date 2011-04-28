all:
	@rebar compile

gc:
	@echo 'Removing all emacs backup files'
	@rm -f *~
	@rm -f */*~
	@for CAT in *; do \
    if [[ -d $${CAT} ]]; then \
      for FEAT in $${CAT}/*; do \
        if [[ -d $${FEAT} && -f $${FEAT}/Makefile ]]; then \
          (cd $${FEAT}; make gc); \
        fi \
      done \
    fi \
  done