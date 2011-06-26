all:
	@rebar compile

dialyze: all
	dialyzer -Wrace_conditions msg/mq_speed/ebin

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

mqrel:
	@echo Generating mq release
	(cd msg/mq_speed/rel; rebar generate)

clean:
	@rebar clean

realclean: clean
	@rebar del-deps
	@rm -rf deps/*
