include ../build/colors.mk

MODULES=$(shell ls . | grep "[^(Makefile)]")

all clean docs:
	@for dir in $(MODULES); do \
		(cd $$dir; if [ -e "SKIP" ]; then  echo $${MY_LRED:-$(LRED)}"skipping \"make $@\" for $$dir"; else ${MAKE} $@; fi); \
		if [ "$$?" -ne "0" ]; then ERROR=$$?; echo "Error Code $$ERROR"; exit $$ERROR; fi; \
		echo -n $(OFF)$(NO_COLOR); \
	done
